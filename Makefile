RUST_LIB_PATH=$(rustc --print target-libdir)

cargo run "sample/let_x.rs" -L "$RUST_LIB_PATH"
