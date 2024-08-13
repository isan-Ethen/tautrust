extern crate t3modules;
use t3modules::*;

fn main() {
    let x = rand_int::<i8>();

    let y = if x > 0 { x } else { x * -1 };

    t3assert(y > 0)
}
