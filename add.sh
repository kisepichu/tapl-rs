cargo test --all && cargo clippy --all --all-features -- -D warnings && cargo fmt --all --check && git add .
