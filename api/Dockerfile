FROM rust:1.64.0-alpine3.16 as builder

RUN apk upgrade --update-cache --available && \
    apk add musl-dev openssl-dev pkgconfig && \
    rm -rf /var/cache/apk/*

WORKDIR /usr/src/api

COPY . .

RUN --mount=type=cache,target=/usr/local/cargo/registry \
    --mount=type=cache,target=/usr/src/api/target \
    cargo install --path .

FROM alpine:latest

ARG APP=/usr/src/app

EXPOSE 8081

ENV TZ=Etc/UTC \
    APP_USER=appuser

RUN addgroup -S $APP_USER \
    && adduser -S -g $APP_USER $APP_USER

# libc6-compat is to make it work on aarm64
RUN apk update \
    && apk add --no-cache ca-certificates tzdata libc6-compat \
    && rm -rf /var/cache/apk/*

COPY --from=builder /usr/local/cargo/bin/api ${APP}/api

RUN chown -R $APP_USER:$APP_USER ${APP}
USER $APP_USER
WORKDIR ${APP}

CMD ["./api"]
