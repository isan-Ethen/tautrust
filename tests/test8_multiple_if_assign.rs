extern crate t3modules;
use t3modules::*;
fn main() {
    let x = rand_int::<i32>();
    let y = if x >= 0 {
        // x; コレを書くと終わる
        let n = if x >= 5 { x - 5 } else { x };
        t3assert(n >= 0);
        n
    } else {
        // x; コレを書くと終わる
        let n = x - 5;
        t3assert(n < 0);
        n * -1
    };
    t3assert(y > 0);
}
