# ---------- Build Dependencies ---------- #
FROM fpco/stack-build:lts-19.29 AS dependencies
COPY stack.yaml package.yaml stack.yaml.lock /build/
WORKDIR /build
RUN stack build --dependencies-only


# ---------- Build Executable ---------- #
FROM fpco/stack-build:lts-19.29 AS builder
COPY --from=dependencies /root/.stack /root/.stack
COPY . /build/
WORKDIR /build
RUN stack install


# ---------- Run in lightweight container ---------- #
FROM ubuntu:latest
WORKDIR /app
COPY --from=builder /build/bin/haskell-web /app/
CMD ["/app/haskell-web"]
