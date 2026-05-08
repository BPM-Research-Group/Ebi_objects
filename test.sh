#!/bin/bash
set -e
set -x
cargo test --verbose
cargo test --verbose --features exactarithmetic,approximatearithmetic
cargo test --verbose --features exactarithmetic
cargo test --verbose --features approximatearithmetic

echo "Ebi was successfully tested"
