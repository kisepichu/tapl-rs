use std::fmt;

use super::r#type::Type;

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Context {
    pub parent: Option<Box<Context>>,
    pub binding: Option<Box<Type>>,
}

#[allow(dead_code)]
impl Context {
    pub fn shift_and_push0(self, ty: Type) -> Context {
        if self.binding.is_some() {
            Context {
                parent: Some(Box::new(self.clone())),
                binding: Some(Box::new(ty)),
            }
        } else {
            Context {
                parent: None,
                binding: Some(Box::new(ty)),
            }
        }
    }

    pub fn concat(self, ctx: Context) -> Context {
        if ctx.binding.is_none() {
            self.clone()
        } else if ctx.parent.is_none() {
            self.shift_and_push0(*ctx.binding.unwrap())
        } else {
            let self_ = if ctx.parent.is_none() {
                self
            } else {
                self.concat(*ctx.parent.unwrap())
            };
            self_.shift_and_push0(*ctx.binding.unwrap())
        }
    }

    pub fn get(&self, var: usize) -> Option<&Type> {
        if var == 0 {
            Some(self.binding.as_ref()?)
        } else if let Some(parent) = &self.parent {
            parent.get(var - 1)
        } else {
            None
        }
    }
}

impl fmt::Display for Context {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut ctx_debug = self;
        let mut i = 0;
        while let Some(parent) = ctx_debug.parent.as_ref() {
            writeln!(f, "{}: {}", i, ctx_debug.get(0).unwrap())?;
            ctx_debug = parent;
            i += 1;
        }
        if let Some(ty) = ctx_debug.get(0) {
            writeln!(f, "{}: {}", i, ty)
        } else {
            writeln!(f, "None")
        }
    }
}
