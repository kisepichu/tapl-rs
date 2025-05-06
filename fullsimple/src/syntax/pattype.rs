use super::{context::Context, r#type::Type};

pub struct PatType {
    pub ty: Type,
    pub add: usize,
    pub context: Context,
}
