fn test(n: i32) {
  match n {
    0..5 => {
      n += 1;
    }
    _ => n += 2,
  }
}
