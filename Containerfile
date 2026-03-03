# gnucashr daemon container
# Multi-stage build: Nix builder -> minimal runtime
#
# Build:
#   podman build -t gnucashr-daemon -f Containerfile .
#
# Run:
#   podman run -v /path/to/finances.gnucash:/data/finances.gnucash:rw \
#              -v /path/to/daemon.json:/config/daemon.json:ro \
#              gnucashr-daemon

# Stage 1: Nix build environment
FROM nixos/nix:latest AS builder

# Enable flakes
RUN echo "experimental-features = nix-command flakes" >> /etc/nix/nix.conf

WORKDIR /src
COPY . .

# Build gnucash-bridge (MCP server) and Dhall configs via Nix
RUN nix build .#gnucashMcp --no-link --print-out-paths > /tmp/mcp-path
RUN cp -rL $(cat /tmp/mcp-path) /tmp/gnucash-mcp

# Also build dhall-json for config rendering
RUN nix build nixpkgs#dhall-json --no-link --print-out-paths > /tmp/dhall-path
RUN cp -rL $(cat /tmp/dhall-path) /tmp/dhall-json

# Stage 2: Minimal runtime
FROM debian:bookworm-slim

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        libsqlite3-0 \
        ca-certificates \
    && rm -rf /var/lib/apt/lists/*

# Copy gnucash-mcp binary and Dhall configs
COPY --from=builder /tmp/gnucash-mcp/bin/gnucash-mcp /usr/local/bin/gnucash-bridge
COPY --from=builder /tmp/gnucash-mcp/share/gnucash-mcp/dhall /usr/local/share/gnucash-mcp/dhall
COPY --from=builder /tmp/dhall-json/bin/dhall-to-json /usr/local/bin/dhall-to-json

# Copy default daemon config
COPY config/gnucashr-daemon.json /config/daemon.json

# Create data and log directories
RUN mkdir -p /data /var/log

# Runtime user
RUN useradd -r -s /bin/false gnucashr
RUN chown -R gnucashr:gnucashr /data /var/log /config
USER gnucashr

VOLUME ["/data", "/config"]

ENTRYPOINT ["gnucash-bridge"]
CMD ["--daemon", "/config/daemon.json"]
