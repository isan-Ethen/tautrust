RUST_LIB_PATH=$(rustc --print target-libdir)

cargo run "sample/test.rs" -L "$RUST_LIB_PATH"
