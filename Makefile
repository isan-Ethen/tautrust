RUST_LIB_PATH=$(rustc --print target-libdir)

cargo run "../tests/thir/tests/only_let.rs" -L "$RUST_LIB_PATH"
