FROM nginx:1.19.6-alpine

RUN apk add certbot certbot-nginx

COPY static/ /home/static
COPY nginx.conf /etc/nginx/nginx.conf

VOLUME /etc/letsencrypt
EXPOSE 80 443
