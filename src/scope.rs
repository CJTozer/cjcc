use anyhow::{bail, Result};
use std::collections::HashMap;

pub struct Scope {
    variables: Vec<HashMap<String, VarData>>,
    index: i32,
}

struct VarData {
    uid: String,
}

impl Scope {
    pub fn top_level() -> Scope {
        Scope {
            variables: vec![HashMap::new()],
            index: 0,
        }
    }

    pub fn start_scope(&mut self) {
        self.variables.push(HashMap::new());
    }

    pub fn end_scope(&mut self) {
        self.variables.pop();
    }

    /// Declares a variable in this scope, and returns a unique reference for this variable at this scope
    pub fn declare_variable(&mut self, var: &String) -> Result<String> {
        let unique_name = String::from(format!("var{}_{}", self.index, &var.clone()));
        self.index += 1;
        match self.variables.last_mut() {
            Some(current) => {
                if current.contains_key(var) {
                    bail!("Variable {} already defined in current scope", var);
                } else {
                    current.insert(
                        var.clone(),
                        VarData {
                            uid: unique_name.clone(),
                        },
                    );
                }
            }
            None => panic!("Trying to declare variable but have no active scope."),
        }

        Ok(unique_name)
    }

    /// Checks for this variable in this and any outer scopes, and returns the unique reference for this variable at the most inner scope
    pub fn check_variable_in_scope(&self, var: &String) -> Result<String> {
        for x in self.variables.iter().rev() {
            if let Some(vardata) = x.get(var) {
                return Ok(vardata.uid.clone());
            }
        }

        bail!(
            "Attempt to use variable {} which is not in this scope.",
            var
        )
    }
}
