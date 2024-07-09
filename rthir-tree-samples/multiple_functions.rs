extern crate t3modules;
use t3modules::*;

fn func1(n: isize) { t3assert(n >= 0); }

fn func2(n: isize) {
  t3assume(n <= 5);
  t3assert(n <= 0);
}

fn main() {
  let n = -1;
  func1(n);
  func2(n);
}
