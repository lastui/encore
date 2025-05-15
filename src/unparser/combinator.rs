use super::unparser::Unparser;

pub trait UnparserCombinator<T> {
    fn unparse(&self, unparser: &mut Unparser, node: &T);
}
