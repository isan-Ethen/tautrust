extern crate t3modules;
use t3modules::*;

fn mutable_ref(ma: &mut i32, mb: &mut i32) {
    let mc = if *ma >= *mb { ma } else { mb };
    *mc += 1;
}

fn main() {
    let mut a = rand_int::<i32>();
    let mut b = rand_int::<i32>();

    mutable_ref(&mut a, &mut b);

    t3assert(a != b);
}
