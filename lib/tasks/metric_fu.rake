begin
  require 'metric_fu'
  MetricFu::Configuration.run do |config| 
    config.rcov[:rcov_opts] << "-Itest" 
  end
rescue LoadError
end