FROM --platform=linux/amd64 debian:bullseye-slim

RUN mkdir -p frontend/
COPY dist frontend/dist
COPY static frontend/static

RUN mkdir -p backend/config
COPY backend backend/
COPY config.toml backend/config/config.toml

COPY entrypoint.sh .
CMD ["bash", "./entrypoint.sh"]
