extern crate t3modules;
use t3modules::*;

fn mutable_ref(ma: &mut i32, mb: &mut i32) {
    let mc = if *ma >= *mb {
        t3drop(mb);
        ma
    } else {
        t3drop(ma);
        mb
    };
    *mc += 1;
    t3drop(mc);
}

fn main() {
    let mut a = rand_int::<i32>();
    let mut b = rand_int::<i32>();

    mutable_ref(&mut a, &mut b);

    t3assert(a != b);
}
