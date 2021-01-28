source 'http://rubygems.org'
ruby '1.9.3'

gem 'rails', '3.2.22.2'

# Bundle edge Rails instead:
#gem 'rails', :git => 'git://github.com/rails/rails.git'

group :assets do
  gem 'sass-rails',   '~> 3.2.3'
  gem 'coffee-rails', '~> 3.2.1'

  gem 'uglifier', '>= 1.0.3'
end

gem 'jquery-rails'
gem 'ejs', '~> 1.0.0'
gem 'eco', '~> 1.0.0'

gem 'oj'

gem 'pg'

gem 'memcachier'
gem 'dalli', :group => :production

gem 'redcarpet'
gem 'will_paginate'
gem 'formtastic'
gem 'friendly_id', '~> 4.0.0'
gem 'rack', '~> 1.4.1'

gem 'aws-s3', :require => 'aws/s3'

gem 'scrambler'

gem 'cancan'

# Fixed version since newer versions don't support Ruby 1.9.
gem 'jwt', '1.5.6'

gem 'delayed_job_active_record'
gem 'delayed_job_web'

gem 'rabl', '0.6.10' # TODO there seems to be a bug with json roots and children

gem 'unicorn'

gem 'newrelic_rpm'

group :development, :test do
  gem 'turn', :require => false
  gem 'rspec-rails'
  gem 'rack-test'
  gem 'shoulda-matchers'
  gem 'capybara'
  gem 'launchy'
  gem 'factory_girl_rails'
  gem 'jasmine'
  gem 'guard-coffeescript'
  gem 'guard-rspec'
  gem 'guard-spork'
  gem 'rb-fsevent'
  gem 'growl'
  gem 'quiet_assets'
  gem 'thin'
end
