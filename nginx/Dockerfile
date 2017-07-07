FROM ubuntu:16.04

RUN apt-get update
RUN apt-get install python-software-properties -y
RUN apt-get install software-properties-common -y
RUN add-apt-repository ppa:certbot/certbot
RUN apt-get update
RUN apt-get install certbot -y
RUN apt-get install nginx -y

COPY public/ /home/assets
COPY nginx/static/ /home/static
COPY nginx/nginx.conf /etc/nginx/nginx.conf

VOLUME /etc/letsencrypt
EXPOSE 80 443
STOPSIGNAL SIGTERM
CMD ["nginx", "-g", "daemon off;"]
