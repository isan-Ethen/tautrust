fn for_expr(n: i32) {
    let mut m = n;
    for i in 0..10 {
        m += i;
    }
}

fn main() {
    let x = 5;
    for_expr(x);
}
