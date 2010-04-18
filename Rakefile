# Add your own tasks in files placed in lib/tasks ending in .rake,
# for example lib/tasks/capistrano.rake, and they will automatically be available to Rake.

require File.expand_path('../config/application', __FILE__)

require 'rake'
require 'rake/testtask'
require 'rake/rdoctask'

Rails::Application.load_tasks

require 'metric_fu'

MetricFu::Configuration.run do |config|
  #define which metrics you want to use
  config.metrics  = [:churn, :stats, :flog, :flay, :reek, :roodi]
end