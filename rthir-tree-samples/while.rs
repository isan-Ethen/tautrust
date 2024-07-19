fn while_expr(n: i32) {
    let mut i = 0;
    while i < n {
        i += 1;
    }
}

fn main() {
    let x = 5;
    while_expr(x);
}
