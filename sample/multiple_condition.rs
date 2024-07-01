fn test(n: usize) {
  if n % 2 == 1 && n % 3 == 0 {
    n += 1;
  } else {
    n += 2;
  }
}
