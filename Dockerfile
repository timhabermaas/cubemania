FROM phusion/baseimage:0.11

RUN apt-get -y update && apt-get install -y \
  build-essential \
  imagemagick \
  libxml2-dev \
  libpq-dev \
  nodejs \
  zlib1g-dev \
  libssl1.0-dev \
  libxslt-dev \
  libreadline6-dev \
  libyaml-dev \
  wget

RUN cd /tmp; wget http://ftp.ruby-lang.org/pub/ruby/1.9/ruby-1.9.3-p551.tar.gz; tar -xvzf ruby-1.9.3-p551.tar.gz; cd ruby-1.9.3-p551/; ./configure --prefix=/usr/local; make; make install

RUN gem install bundler -v 1.11.2

RUN mkdir /cubemania
WORKDIR /cubemania
ADD Gemfile* /cubemania/
RUN bundle install

ADD . /cubemania

# required for delayed job
RUN mkdir -p tmp/pids
