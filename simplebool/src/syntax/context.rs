use super::ty::Type;

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Context {
    pub parent: Option<Box<Context>>,
    pub binding: Option<Box<Type>>,
}

#[allow(dead_code)]
impl Context {
    pub fn push(&mut self, ty: Type) -> Context {
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
