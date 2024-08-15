extern crate t3modules;
use t3modules::*;

fn take_max<'a>(a: &'a mut i32, b: &'a mut i32) -> &'a mut i32 {
    let c = if *a >= *b {
        t3assert(*a >= *b);
        t3drop(b);
        a
    } else {
        t3assert(*a < *b);
        t3drop(a);
        b
    };
    c
}

fn main() {
    let mut a = rand_int::<i32>();
    let mut b = rand_int::<i32>();

    let ma = &mut a;
    let mb = &mut b;

    let mc = take_max(ma, mb);

    *mc += 1;

    t3drop(mc);
    t3assert(a != b);
}
