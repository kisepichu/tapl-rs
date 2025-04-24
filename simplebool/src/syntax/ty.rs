#[allow(dead_code)] // todo
pub enum Type {
    Arr(Box<Type>, Box<Type>),
    Bool,
}
