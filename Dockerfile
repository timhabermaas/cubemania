FROM phusion/baseimage:0.9.18

RUN apt-get -y update && apt-get install -y \
  build-essential \
  imagemagick \
  ruby 1.9.3 \
  libxml2-dev \
  libxslt1-dev \
  libpq-dev \
  nodejs

RUN gem install bundler -v 1.11.2

RUN mkdir /cubemania
WORKDIR /cubemania
ADD Gemfile* /cubemania/
RUN bundle install

ADD . /cubemania

# required for delayed job
RUN mkdir -p tmp/pids
