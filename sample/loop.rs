fn loop_expr(n: i32) {
  loop {
    if n > 10 {
      break;
    }
    n += 1;
  }
}

fn main() {
  let x = 5;
  loop_expr(x);
}
