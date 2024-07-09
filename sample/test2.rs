extern crate t3modules;
use t3modules::*;

fn return_expr(n: i32) -> i32 {
    let m = n + 1;
    return m;
}

fn main() {
    let x = 5;
    let y = return_expr(x);
    t3assert(y > x);
}
