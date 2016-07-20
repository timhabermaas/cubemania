FROM ubuntu

RUN apt-get update -y && apt-get install -y libpq-dev

COPY ./dist/api-exe /app/api-exe
WORKDIR /app
CMD /app/api-exe
