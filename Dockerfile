ARG OTP_VERSION

# Build the release
FROM docker.io/library/erlang:${OTP_VERSION} AS builder
SHELL ["/bin/bash", "-o", "pipefail", "-c"]

# Install cmake
RUN apt-get --yes update \
    && apt-get --yes --no-install-recommends install \
        cmake \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Install thrift compiler
ARG THRIFT_VERSION
ARG TARGETARCH
RUN wget -q -O- "https://github.com/valitydev/thrift/releases/download/${THRIFT_VERSION}/thrift-${THRIFT_VERSION}-linux-${TARGETARCH}.tar.gz" \
    | tar -xvz -C /usr/local/bin/

# Copy sources
RUN mkdir /build
COPY . /build/

# Build the release
WORKDIR /build
RUN rebar3 compile && \
    rebar3 as prod release

# Make a runner image
FROM docker.io/library/erlang:${OTP_VERSION}-slim

ARG SERVICE_NAME
ARG USER_UID=1001
ARG USER_GID=$USER_UID

# Set env
ENV CHARSET=UTF-8
ENV LANG=C.UTF-8

# Set runtime
WORKDIR /opt/${SERVICE_NAME}

COPY --from=builder /build/_build/prod/rel/${SERVICE_NAME} /opt/${SERVICE_NAME}

RUN echo "#!/bin/sh" >> /entrypoint.sh && \
    echo "exec /opt/${SERVICE_NAME}/bin/${SERVICE_NAME} foreground" >> /entrypoint.sh && \
    chmod +x /entrypoint.sh

# Setup user
RUN groupadd --gid ${USER_GID} ${SERVICE_NAME} && \
    useradd --uid ${USER_UID} --gid ${USER_GID} -M ${SERVICE_NAME}
USER ${SERVICE_NAME}

ENTRYPOINT []
CMD ["/entrypoint.sh"]

EXPOSE 8022
