extern crate t3modules;
use t3modules::*;

fn return_expr(n: i32) -> i32 {
    if n >= 0 {
        n
    } else {
        n * -1
    }
}

fn main() {
    let x = rand_int::<i32>();
    let y = return_expr(x);
    t3assert(y >= 0);
}
