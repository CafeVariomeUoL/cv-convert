FROM alpine:3.12 as build-tooling

RUN apk add ghc

ENV STACK_VERSION=2.3.1
ENV STACK_SHA256="4bae8830b2614dddf3638a6d1a7bbbc3a5a833d05b2128eae37467841ac30e47  stack-${STACK_VERSION}-linux-x86_64-static.tar.gz"

# Download, verify, and install stack
RUN echo "Downloading and installing stack" &&\
    cd /tmp &&\
    wget -P /tmp/ "https://github.com/commercialhaskell/stack/releases/download/v${STACK_VERSION}/stack-${STACK_VERSION}-linux-x86_64-static.tar.gz" &&\
    if ! echo -n "${STACK_SHA256}" | sha256sum -c -; then \
        echo "stack-${STACK_VERSION} checksum failed" >&2 &&\
        exit 1 ;\
    fi ;\
    tar -xvzf /tmp/stack-${STACK_VERSION}-linux-x86_64-static.tar.gz &&\
    cp -L /tmp/stack-${STACK_VERSION}-linux-x86_64-static/stack /usr/bin/stack &&\
    rm /tmp/stack-${STACK_VERSION}-linux-x86_64-static.tar.gz &&\
    rm -rf /tmp/stack-${STACK_VERSION}-linux-x86_64-static

# # NOTE: 'stack --docker' needs bash + usermod/groupmod (from shadow)
RUN apk add --no-cache bash shadow openssh-client tar

RUN stack config set system-ghc --global true

RUN apk add --no-cache alpine-sdk zlib-dev pcre-dev zlib-static