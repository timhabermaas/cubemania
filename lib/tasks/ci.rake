namespace :ci do
  task :copy_db_yml do
    system("cp #{Rails.root}/config/database.yml.sample #{Rails.root}/config/database.yml")
  end
  
  task :bundle_install do
    system("bundle install")
  end
  
  desc "Prepare for CI and run entire test suite"
  task :build => ['ci:bundle_install', 'ci:copy_db_yml', 'db:setup', 'spec'] do
  end
end