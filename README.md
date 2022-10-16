# SHIRT SIZE
shirt size is a simple full stack app for voting on shirt sizes for ticket estimation

# running
run 
```
bash build.sh
docker run -e API_KEY={SOME_API_KEY_MORE_THAN_64_BYTES} -e RUST_LOG=DEBUG,tower-http=debug -d -p 8000:8000 shirt-size:latest
```
