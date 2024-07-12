extern crate t3modules;
use t3modules::*;
fn main() {
    let x = rand_int::<i32>();
    let y = if x >= 0 {
        x;
        let n = x + 5;
        n
    } else {
        x;
        let n = x - 5;
        n * -1
    };
    t3assert(y > 0);
}
