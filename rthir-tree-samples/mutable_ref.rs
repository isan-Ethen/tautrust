fn mutable_ref(a: &mut usize) { *a += 1; }

fn main() {
    let mut a = 0;

    mutable_ref(&mut a);
}
