pub mod advent0 {
    use crate::input::input;

    pub fn sum_of_numbers() -> i32 {
        input::read_input(0)
            .iter()
            .map(|str: &String| str.parse::<i32>().unwrap())
            .sum()
    }
}
