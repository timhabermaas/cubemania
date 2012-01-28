source 'http://rubygems.org'

gem 'rails', '3.2.0'

# Bundle edge Rails instead:
#gem 'rails', :git => 'git://github.com/rails/rails.git'

group :assets do
  gem 'sass-rails',   '~> 3.2.3'
  gem 'coffee-rails', '~> 3.2.1'

  gem 'uglifier', '>= 1.0.3'
end

gem 'mysql'

gem 'jquery-rails'

gem 'json'

gem 'pg', :group => :production
gem 'sqlite3', :group => [:development, :test]

gem 'RedCloth'
gem 'will_paginate'
gem 'formtastic'
gem 'friendly_id', '~> 4.0.0'
gem 'omniauth-twitter'
gem 'omniauth-facebook'
gem 'rack', :git => 'https://github.com/rack/rack.git', :ref =>'e20baec005238f9876281c0d083fe5a4e01aa034' # FIXME https://github.com/intridea/omniauth/issues/562
gem 'fb_graph'

gem 'aws-s3', :require => 'aws/s3'

gem 'scrambler'

gem 'cancan'

gem 'delayed_job_active_record'
gem 'delayed_job_web'

gem 'grape'

gem 'thin'

gem 'newrelic_rpm'

group :development, :test do
  gem 'turn', :require => false
  gem 'rspec-rails'
  gem 'shoulda-matchers'
  gem 'capybara'
  gem 'launchy'
  gem 'factory_girl_rails'
  gem 'spork', '~> 0.9.0.rc'
  gem 'timecop'
end