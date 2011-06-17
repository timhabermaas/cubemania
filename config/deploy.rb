$:.unshift(File.expand_path('./lib', ENV['rvm_path'])) # Add RVM's lib directory to the load path.
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

after "deploy", "deploy:bundle_gems"
after "deploy:bundle_gems", "deploy:restart"

namespace :deploy do
  task :bundle_gems do
    run "cd #{deploy_to}/current && bundle install"
  end
  task :start do ; end
  task :stop do ; end
  task :restart, :roles => :app, :except => { :no_release => true } do
    run "thin restart -C /etc/thin/cubemania_beta.yml"
  end
end