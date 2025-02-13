extern crate t3modules;
use t3modules::*;

fn test_function(n: i32) -> i32 {
    t3assert(n > 0);

    let m = n * n;
    t3assert(m > 0);

    m
}

fn main() {
    let x = rand_int::<i32>();
    t3assume(x > 0);

    let y = test_function(x);
    t3assert(y > 0);
}
