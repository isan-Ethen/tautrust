extern crate t3modules;
use t3modules::*;

fn main() {
    let mut x = rand_bool();
    t3assume(x == true);

    x = !x;
    t3assert(x == false);
    let mut y = rand_int::<i32>();
    t3assume(y == 5);

    y = -y;
    t3assert(y == -5);
}
