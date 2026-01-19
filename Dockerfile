# Simple production image - Elm is built locally before docker build
FROM nginx:alpine

# Copy pre-built static files (elm.js is already in static/ from local build)
COPY static/ /usr/share/nginx/html/

EXPOSE 80

CMD ["nginx", "-g", "daemon off;"]
