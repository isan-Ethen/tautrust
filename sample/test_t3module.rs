extern crate t3modules;
use t3modules::*;

fn test_t3modules(n: isize) {
    t3assert(n >= 5);
    t3assert(n > 0);
}

fn main() {
    let x = 5;
    test_t3modules(x);
}
