fn match_expr(n: i32) {
  match n {
    0..5 => {
      n += 1;
    }
    _ => n += 2,
  }
}

fn main() {
  let x = 5;
  match_expr(x);
}
