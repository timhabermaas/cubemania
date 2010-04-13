namespace :cache do
  desc 'Clear memcached'
  task :clear => :environment do
    ActionController::Base.cache_store.clear
  end
end