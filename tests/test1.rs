extern crate t3modules;
use t3modules::*;

fn let_m(n: i32) {
    let m = n * n;
    t3assert(m >= 9);
}

fn main() {
    let x = rand_int();
    t3assume(x >= 3);
    let_m(x);
}
