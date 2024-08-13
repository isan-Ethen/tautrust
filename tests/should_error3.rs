extern crate t3modules;
use t3modules::*;

fn main() {
    let mut x = rand_int::<i8>();
    t3assume(x * x > 1);

    if x < 0 {
        x *= -1;
    }

    t3assert(x > 0)
}
