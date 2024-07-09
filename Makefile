RUST_LIB_PATH=$(rustc --print target-libdir)
T3MODULES="./t3modules/target/release/libt3modules.rlib"

for file in tests/test*.rs; do
    cargo run "$file" -L "$RUST_LIB_PATH" --extern t3modules="$T3MODULES"
done
