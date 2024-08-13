extern crate t3modules;
use t3modules::*;
fn main() {
    let x = rand_int::<i32>();
    t3assume(x > 1 + 1 - (4 % 3) / 1);
    t3assert(x != 0);
}
