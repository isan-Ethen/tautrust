fn multiple_condition(n: usize) {
  if n % 2 == 1 && n % 3 == 0 {
    n += 1;
  } else {
    n += 2;
  }
}

fn main() {
  let x = 9;
  multiple_condition(x);
}
