$:.unshift(File.expand_path('./lib', ENV['rvm_path'])) # Add RVM's lib directory to the load path.
$:.unshift(File.join(File.dirname(__FILE__), 'deploy'))
require "capistrano_database_yml"
require "rvm/capistrano"

set :rvm_ruby_string, '1.9.2'
# set :rvm_type, :user if you don't want to use systemwide install, recommend

set :application, "cubemania_beta"
set :repository,  "git@timhabermaas.beanstalkapp.com:/cubemania.git"

set :scm, :git
set :branch, "rails31"
set :scm_verbose, true

ssh_options[:forward_agent] = true # use local ssh keys

set :user, "root"
set :use_sudo, false
set :deploy_to, "/web/#{application}"
set :deploy_via, :remote_cache

role :web, "72.14.185.22"                          # Your HTTP server, Apache/etc
role :app, "72.14.185.22"                          # This may be the same as your `Web` server
role :db,  "72.14.185.22", :primary => true        # This is where Rails migrations will run

after "deploy:finalize_update", "deploy:bundle_gems"
after "deploy:bundle_gems", "deploy:db:setup"
after "deploy:db:setup", "deploy:symlink_shared"
after "deploy:symlink_shared", "deploy:precompile_assets"

namespace :deploy do
  task :bundle_gems do
    run "cd #{release_path} && bundle install"
  end
  desc "Compile assets"
  task :precompile_assets do
    run "cd #{release_path}; RAILS_ENV=production bundle exec rake assets:precompile"
  end
  task :start do ; end
  task :stop do ; end
  task :restart, :roles => :app, :except => { :no_release => true } do
    run "thin restart -C /etc/thin/cubemania_beta.yml"
  end
  desc "Symlink shared configs and folders on each release"
  task :symlink_shared, :except => { :no_release => true } do
    run "ln -nfs #{shared_path}/assets #{release_path}/public/assets"
    run "ln -nfs #{shared_path}/config/database.yml #{release_path}/config/database.yml"
  end
end