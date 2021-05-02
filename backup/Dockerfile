FROM alpine:3.4

RUN apk --update --upgrade add postgresql python3 py-pip groff less && rm -rf /var/cache/apk/*
RUN pip install awscli

RUN mkdir /data
COPY backup.sh /root/backup.sh
COPY restore.sh /root/restore.sh

COPY crontab /var/spool/cron/crontabs/root

WORKDIR /root

CMD crond -l 2 -f
