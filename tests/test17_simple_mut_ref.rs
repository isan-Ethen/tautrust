extern crate t3modules;
use t3modules::*;

fn main() {
    let mut a = rand_int::<i32>();
    let mut b = rand_int::<i32>();
    t3assume(b == 10);
    let ma = &mut a;
    *ma *= 0;

    b += *ma;
    t3assert(b == 10);

    t3drop(ma);
    t3assert(a == 0);
}
