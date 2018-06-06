FROM alpine:3.4

RUN apk --update --upgrade add tarsnap postgresql
COPY tarsnap.conf /etc/tarsnap/tarsnap.conf

RUN mkdir /data
COPY backup.sh /root/backup.sh
COPY restore.sh /root/restore.sh

COPY crontab /var/spool/cron/crontabs/root

VOLUME /root/keys
VOLUME /root/tarsnap-cache

WORKDIR /root

CMD crond -l 2 -f
