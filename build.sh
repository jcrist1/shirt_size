cd backend 
export  TARGET_CC=x86_64-unknown-linux-gnu-gcc
cargo build --release --target=x86_64-unknown-linux-gnu
cd ../frontend
trunk build --release
cd ..
cp -r frontend/dist docker/
mkdir -p docker/backend
cp -r target/x86_64-unknown-linux-gnu/release/main docker/backend/shirt-size-server
cd docker
docker build -t shirt-size:latest . 
