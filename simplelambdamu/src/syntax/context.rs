use crate::syntax::term::Info;

use super::r#type::Type;

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Context {
    pub parent: Option<Box<Context>>,
    pub binding: Option<Box<Type>>,
    pub info: Option<Info>,
}

#[allow(dead_code)]
impl Context {
    pub fn push(&mut self, ty: Type, info: Info) -> Context {
        if self.binding.is_some() {
            Context {
                parent: Some(Box::new(self.clone())),
                binding: Some(Box::new(ty)),
                info: Some(info),
            }
        } else {
            Context {
                parent: None,
                binding: Some(Box::new(ty)),
                info: Some(info),
            }
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

    pub fn get_info(&self, var: usize) -> Option<&Info> {
        if var == 0 {
            self.info.as_ref()
        } else if let Some(parent) = &self.parent {
            parent.get_info(var - 1)
        } else {
            None
        }
    }
}
