extern crate t3modules;
use t3modules::*;

fn test_t3modules(n: isize) {
  t3assume(n >= 5);
  t3assert(n > 0);
}
