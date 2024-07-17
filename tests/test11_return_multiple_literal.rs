extern crate t3modules;
use t3modules::*;

fn main() {
    let x = rand_int::<i32>();
    let y = if x >= 0 { 5 } else { 8 };
    t3assert(y > 0);
}
