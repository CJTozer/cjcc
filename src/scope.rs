use anyhow::{bail, Result};
use std::collections::HashMap;

#[derive(Debug)]
pub struct Scope {
    /// Variables can be shadowed so have a stack of scopes
    variables: Vec<HashMap<String, VarData>>,
    /// Functions are globally scoped
    functions: HashMap<String, FunData>,
    index: i32,
}

#[derive(Debug)]
struct VarData {
    uid: String,
}

#[derive(Debug)]
struct FunData {
    n_params: i32, // Currently only support int32 parameters
}

impl Scope {
    pub fn top_level() -> Scope {
        Scope {
            variables: vec![HashMap::new()],
            functions: HashMap::new(),
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

    /// Declares a function
    pub fn declare_function(&mut self, fun: &String, n_params: i32) -> Result<()> {
        // It's acceptable for a function declaration to come after the definition (or to be declared more than once).
        // Check it has the same parameters - which is the same as `define_function` so default to that.
        self.define_function(fun, n_params)
    }

    /// Defines a function - it may or may not have already been defined.
    pub fn define_function(&mut self, fun: &String, n_params: i32) -> Result<()> {
        // TODO get context on file/line
        if let Some(fun_data) = self.functions.get(fun) {
            if fun_data.n_params == n_params {
                // Definition matches declaration
                Ok(())
            } else {
                bail!("Function definition for '{}' ({} arguments) does not match declaration ({} arguments)", fun, n_params, fun_data.n_params)
            }
        } else {
            // Not already declared, make this the implicit declaration.
            let fun_data = FunData { n_params: n_params };
            self.functions.insert(fun.clone(), fun_data);
            Ok(())
        }
    }

    /// Checks that a function exists with the same parameters
    pub fn check_function_call(&self, fun: &String, n_params: i32) -> Result<()> {
        if let Some(fun_data) = self.functions.get(fun) {
            if fun_data.n_params == n_params {
                Ok(())
            } else {
                bail!(
                    "Calling function '{}' with {} parameters, expected {}.",
                    fun,
                    n_params,
                    fun_data.n_params
                )
            }
        } else {
            println!("{:?}", self);
            bail!("No function '{}' defined in this scope.", fun)
        }
    }
}
