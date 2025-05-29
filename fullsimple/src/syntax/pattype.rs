use super::{context::Context, r#type::Type};

pub struct PatType {
    pub ty: Type,
    pub add: usize,
    #[allow(unused)]
    pub context: Context,
}
