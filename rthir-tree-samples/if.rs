fn if_expr(n: i32) {
  if n % 2 == 1 {
    n += 1;
  } else {
    n /= 2;
  }
}

fn main() {
  let x = 5;
  if_expr(x);
}
