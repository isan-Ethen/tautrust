extern crate t3modules;
use t3modules::*;

fn main() {
    let mut x = rand_int::<i32>();
    let mut y = rand_int::<i32>();
    t3assume(x == y);

    let mut i = 0;

    invariant(i <= x);
    while i < x {
        i += 1;
    }

    t3assert(i == y)
}
