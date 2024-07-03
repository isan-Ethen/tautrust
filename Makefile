RUST_LIB_PATH=$(rustc --print target-libdir)
T3MODULES="./t3modules/target/release/libt3modules.rlib"

for file in sample/*.rs; do
    name=$(basename "${file%.rs}")
    cargo run "sample/$name.rs" -L "$RUST_LIB_PATH" --extern t3modules="$T3MODULES" | grep -v "Tautrust!" > "sample/$name.tree"
done
