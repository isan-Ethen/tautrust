extern crate t3modules;
use t3modules::*;

fn main() {
    let x = rand_int::<i8>();
    t3assume(x >= 3);
    let m = x * x;
    t3assert(m >= 9);
}
