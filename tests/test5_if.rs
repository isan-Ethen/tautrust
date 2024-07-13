extern crate t3modules;
use t3modules::*;
fn main() {
    let x = rand_int::<i32>();
    let mut m = rand_int::<i32>();
    if x >= 0 {
        m = x + 5;
        t3assert(m >= 0);
    } else {
        m = x - 5;
        t3assert(m < 0);
    }
    t3assert(m * m > 0);
}
