## BUILD
FROM haskell:9.2.5 as build

RUN mkdir /opt/build
WORKDIR /opt/build

COPY ./stack.yaml ./sunrisebot-haskell.cabal ./stack.yaml.lock ./
RUN stack build --dependencies-only

WORKDIR /opt/build
COPY . .
RUN stack build
RUN mv "$(stack path --local-install-root --system-ghc)/bin" /opt/build/bin

## RUN
FROM debian:buster as deploy

RUN mkdir -p /opt/app
WORKDIR /opt/app
COPY --from=build /opt/build/bin .

RUN apt-get update && apt-get install -y curl

ENV SUPERCRONIC_URL=https://github.com/aptible/supercronic/releases/download/v0.1.12/supercronic-linux-amd64 \
    SUPERCRONIC=supercronic-linux-amd64 \
    SUPERCRONIC_SHA1SUM=048b95b48b708983effb2e5c935a1ef8483d9e3e

RUN curl -fsSLO "$SUPERCRONIC_URL" \
 && echo "${SUPERCRONIC_SHA1SUM}  ${SUPERCRONIC}" | sha1sum -c - \
 && chmod +x "$SUPERCRONIC" \
 && mv "$SUPERCRONIC" "/usr/local/bin/${SUPERCRONIC}" \
 && ln -s "/usr/local/bin/${SUPERCRONIC}" /usr/local/bin/supercronic

COPY crontab crontab

CMD supercronic ./crontab
