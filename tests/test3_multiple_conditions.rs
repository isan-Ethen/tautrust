extern crate t3modules;
use t3modules::*;

fn multiple_conditions(n: i32) { t3assert(n * 5 >= 0 && n / 10 > 0); }

fn main() {
    let x = rand_int();
    t3assume(x > 0);
    multiple_conditions(x);
}
