extern crate t3modules;
use t3modules::*;

fn test_t3assert_int(n: isize) { t3assert(n > 0); }

fn test_t3assert_float(m: f64) { t3assert(m > 0.0); }

fn main() {
    let x = rand_int::<isize>();
    t3assume(x >= 5);
    test_t3assert_int(x);
    let y = rand_float::<f64>();
    t3assume(y >= 0.0);
    let z = y + 1.0;
    test_t3assert_float(z);
}
