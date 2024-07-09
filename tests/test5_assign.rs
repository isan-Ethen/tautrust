extern crate t3modules;
use t3modules::*;

fn main() {
    let mut x = rand_int::<i32>();
    t3assume(x >= 0);
    x += 10;
    t3assert(x >= 10);
    let y = 50 + x;
    x = y;
    t3assert(x >= 60);
}
