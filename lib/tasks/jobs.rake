namespace :jobs do
  desc "Work off 100 delayed_job jobs"
  task :work_off => :environment do
    Delayed::Worker.new(:quiet => false).work_off
  end
end
