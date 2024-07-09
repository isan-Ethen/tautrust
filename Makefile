RUST_LIB_PATH=$(rustc --print target-libdir)
T3MODULES="./t3modules/target/release/libt3modules.rlib"

for num in sample/test*.rs; do
    name=$(basename "test${num%.rs}")
    cargo run "sample/$name.rs" -L "$RUST_LIB_PATH" --extern t3modules="$T3MODULES"
done
