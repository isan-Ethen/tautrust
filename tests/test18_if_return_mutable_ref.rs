extern crate t3modules;
use t3modules::*;

fn main() {
    let mut a = rand_int::<i32>();
    let mut b = rand_int::<i32>();

    let ma = &mut a;
    let mb = &mut b;

    let mc = if *ma >= *mb { ma } else { mb };
    *mc += 1;

    t3assert(a != b);
}
