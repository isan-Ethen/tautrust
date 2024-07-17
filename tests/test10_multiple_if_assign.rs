extern crate t3modules;
use t3modules::*;

fn main() {
    let x = rand_int::<i32>();
    let y = if x >= 0 {
        // x; コレを書くと終わる
        if x >= 5 {
            10
        } else {
            5
        }
    } else {
        // x; コレを書くと終わる
        let n = x - 5;
        t3assert(n < 0);
        n * -1
    };
    t3assert(y > 0);
}
