extern crate t3modules;
use t3modules::*;

fn main() {
    let a = rand_int::<i32>();
    let b;

    if a >= 0 {
        let c = rand_int::<i32>();
        t3assume(c > 0);
        b = a + c;
        t3assert(b > 0);
    } else {
        let c = rand_int::<i32>();
        t3assume(c < 0);
        b = a + c;
        t3assert(b < 0);
    }

    t3assert(b * b > 0);
}
