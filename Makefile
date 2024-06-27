RUST_LIB_PATH=$(rustc --print target-libdir)

for file in sample/*.rs; do
    name=$(basename "${file%.rs}")
    cargo run "sample/$name.rs" -L "$RUST_LIB_PATH" | grep -v "Tautrust!" > "sample/$name.tree"
done
