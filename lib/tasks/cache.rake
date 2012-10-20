namespace :cache do
  desc 'Clear memcached'
  task :clear => :environment do
    ActionController::Base.cache_store.clear
  end

  desc "Recalculate user's wasted_time cache"
  task :update_wasted_time => :environment do
    Rake::Task["users:calculate_wasted_time"].invoke if Time.now.monday?
  end
end
