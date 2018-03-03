pub mod tal_0;
pub mod tal_1;
pub mod system_f;

pub trait TypeCheck {
    type Input;
    type Output;

    fn type_of(self, input: Self::Input) -> Self::Output;
}
