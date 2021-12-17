# syntax=docker/dockerfile:experimental

FROM ekidd/rust-musl-builder:1.51.0 as builder

RUN mkdir /home/rust/api
WORKDIR /home/rust/api

COPY --chown=rust:rust . ./

RUN --mount=type=cache,target=/home/rust/.cargo/registry,uid=1000,gid=1000 \
    --mount=type=cache,target=/home/rust/api/target,uid=1000,gid=1000 \
    cargo build --release && cp target/x86_64-unknown-linux-musl/release/api ./api

FROM alpine:latest

ARG APP=/usr/src/app

EXPOSE 8081

ENV TZ=Etc/UTC \
    APP_USER=appuser

RUN addgroup -S $APP_USER \
    && adduser -S -g $APP_USER $APP_USER

RUN apk update \
    && apk add --no-cache ca-certificates tzdata \
    && rm -rf /var/cache/apk/*

COPY --from=builder /home/rust/api/api ${APP}/api

RUN chown -R $APP_USER:$APP_USER ${APP}
USER $APP_USER
WORKDIR ${APP}

CMD ["./api"]
