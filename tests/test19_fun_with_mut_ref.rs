extern crate t3modules;
use t3modules::*;

fn abs(m: &mut i32) {
    if *m < 0 {
        *m *= -1;
    }
    t3assert(*m >= 0);
    t3drop(m);
}

fn main() {
    let mut a = rand_int::<i32>();

    let ma = &mut a;

    abs(ma);

    t3assert(a >= 0);
}
