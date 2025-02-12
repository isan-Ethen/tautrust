extern crate t3modules;
use t3modules::*;

fn test_function(n: i32) { t3assert(n * n > 0); }

fn main() {
    let x = rand_int::<i32>();
    t3assume(x > 0);
    test_function(x);
}
