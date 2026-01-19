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
