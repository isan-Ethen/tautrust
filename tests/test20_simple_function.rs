extern crate t3modules;
use t3modules::*;

fn simple_function(n: i32) {
    let m = n + 1;
    t3assert(m >= 6);
}

fn main() {
    let x = rand_int::<i32>();
    let y = rand_int::<i32>();
    t3assume(x >= 5);
    t3assume(y >= 0);
    simple_function(x);
    t3assert(y >= 0);
}
