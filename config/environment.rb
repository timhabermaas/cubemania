# Load the rails application
require File.expand_path('../application', __FILE__)

# Initialize the rails application
Cubemania::Application.initialize!

# set timeout for database queries to 20s
ActiveRecord::Base.connection.select_all('set statement_timeout to 20000') if Rails.env == 'production'