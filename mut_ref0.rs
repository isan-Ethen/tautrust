extern crate t3modules;
use t3modules::*;

fn main() {
    let mut a = rand_int::<i32>();
    let mut b = rand_int::<i32>();

    let ma = &mut a;
    let mb = &mut b;

    let mc = if *ma >= *mb {
        t3drop(mb);
        ma
    } else {
        t3drop(ma);
        mb
    };

    *mc += 1;

    t3drop(mc);
    t3assert(a != b);
}
