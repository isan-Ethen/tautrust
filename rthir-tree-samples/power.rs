extern crate t3modules;
use t3modules::*;

fn main() {
    let x = rand_int::<i32>();

    t3assume(x != 0);

    x *= x;

    t3assert(x > 0);
}
