FROM node:20-alpine AS builder

WORKDIR /app

# Install elm
COPY package*.json ./
RUN npm ci

# Copy source and build
COPY elm.json ./
COPY src/ ./src/
RUN npx elm make src/Main.elm --optimize --output=elm.js

# Production image
FROM nginx:alpine

# Copy static files
COPY static/ /usr/share/nginx/html/
COPY --from=builder /app/elm.js /usr/share/nginx/html/

EXPOSE 80

CMD ["nginx", "-g", "daemon off;"]
