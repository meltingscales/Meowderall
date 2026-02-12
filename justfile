# List available recipes
help:
    @just --list

# Build the Elm project
build:
    npx elm make src/Main.elm --output=static/elm.js

# Build optimized for production
build-release:
    npx elm make src/Main.elm --optimize --output=static/elm.js

# Run development server (requires python3)
dev:
    just build
    cd static && python3 -m http.server 8081

# Format Elm code (requires elm-format)
fmt:
    npx elm-format src/ --yes

# Clean build artifacts
clean:
    rm -f static/elm.js

# Docker operations
# ================

# Build Docker image
docker-build:
    docker build -t meowderall:latest .

# Run Docker container locally
docker-run port="8080":
    docker run -p {{port}}:80 meowderall:latest

# Stop all running containers for this project
docker-stop:
    docker ps -q --filter ancestor=meowderall:latest | xargs -r docker stop

# GCP Deployment
# ==============

GCP_PROJECT := env_var_or_default("GCP_PROJECT", "meowderall")
GCP_REGION := env_var_or_default("GCP_REGION", "us-central1")
SERVICE_NAME := "meowderall"

# Build and push Docker image to Google Container Registry
gcp-push:
    docker build -t gcr.io/{{GCP_PROJECT}}/{{SERVICE_NAME}}:latest .
    docker push gcr.io/{{GCP_PROJECT}}/{{SERVICE_NAME}}:latest

# Deploy to Google Cloud Run
gcp-deploy:
    gcloud run deploy {{SERVICE_NAME}} \
        --image gcr.io/{{GCP_PROJECT}}/{{SERVICE_NAME}}:latest \
        --platform managed \
        --region {{GCP_REGION}} \
        --allow-unauthenticated \
        --port 80 \
        --memory 128Mi \
        --cpu 1 \
        --project {{GCP_PROJECT}}

# Build, push, and deploy to GCP in one command
gcp-deploy-all:
    just build-release
    just gcp-push
    just gcp-deploy

# View Cloud Run service logs
gcp-logs:
    gcloud run services logs read {{SERVICE_NAME}} --region {{GCP_REGION}} --project {{GCP_PROJECT}}

# Get Cloud Run service URL
gcp-url:
    gcloud run services describe {{SERVICE_NAME}} --region {{GCP_REGION}} --project {{GCP_PROJECT}} --format 'value(status.url)'

# Systemd Service Setup (for GCP VM)
# ====================================

# Install as systemd service running on port 3001
# Run with sudo
systemd-install:
    #!/usr/bin/env bash
    set -euo pipefail

    if [[ $EUID -ne 0 ]]; then
        echo "Error: This recipe must be run as root (use sudo)."
        exit 1
    fi

    SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
    SERVICE_NAME="meowderall"
    PORT="${PORT:-3001}"
    REPO_DIR="${REPO_DIR:-${SCRIPT_DIR}}"
    USER="${SUDO_USER:-root}"

    echo "Installing systemd service: ${SERVICE_NAME}"

    # Build Elm app first
    echo "Building Elm app..."
    cd "${REPO_DIR}"
    just build-release

    # Copy and template service file
    sed -e "s|USER_PLACEHOLDER|${USER}|g" \
        -e "s|REPO_DIR_PLACEHOLDER|${REPO_DIR}|g" \
        "${SCRIPT_DIR}/systemd/${SERVICE_NAME}.service" \
        > /etc/systemd/system/${SERVICE_NAME}.service

    # Reload systemd and enable service
    systemctl daemon-reload
    systemctl enable ${SERVICE_NAME}
    systemctl restart ${SERVICE_NAME}

    echo "Service installed and started on port ${PORT}!"
    echo ""
    echo "Commands:"
    echo "  sudo systemctl status ${SERVICE_NAME}"
    echo "  sudo systemctl restart ${SERVICE_NAME}"
    echo "  sudo journalctl -u ${SERVICE_NAME} -f"

# Uninstall systemd service
# Run with sudo
systemd-uninstall:
    #!/usr/bin/env bash
    SERVICE_NAME="meowderall"

    if [[ $EUID -ne 0 ]]; then
        echo "Error: This recipe must be run as root (use sudo)."
        exit 1
    fi

    echo "Stopping and disabling ${SERVICE_NAME}..."
    systemctl stop ${SERVICE_NAME} 2>/dev/null || true
    systemctl disable ${SERVICE_NAME} 2>/dev/null || true
    rm -f /etc/systemd/system/${SERVICE_NAME}.service
    systemctl daemon-reload
    echo "Service uninstalled."

# Show service status
systemd-status:
    #!/usr/bin/env bash
    SERVICE_NAME="${SERVICE_NAME:-meowderall}"
    systemctl status ${SERVICE_NAME}

# View service logs
systemd-logs:
    #!/usr/bin/env bash
    SERVICE_NAME="${SERVICE_NAME:-meowderall}"
    journalctl -u ${SERVICE_NAME} -f

# Restart the service
systemd-restart:
    #!/usr/bin/env bash
    SERVICE_NAME="${SERVICE_NAME:-meowderall}"
    systemctl restart ${SERVICE_NAME}
    systemctl status ${SERVICE_NAME}
