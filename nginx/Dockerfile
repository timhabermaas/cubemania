FROM nginx

COPY public/ /home/assets
COPY nginx/nginx.conf /etc/nginx/nginx.conf

VOLUME /cert
EXPOSE 80 443
