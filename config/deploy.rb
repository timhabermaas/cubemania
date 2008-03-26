set :application, 'cubemania'
set :user, 'fortytwo'
set :home, "/users/home/#{user}"
set :mongrel_port, 4008
set :rails_env, 'production'
set :domain, 'cubemania.org'
set :application_path , "#{home}/domains/#{domain}/web"

role :app, 'spring.joyent.us'

namespace :deploy do
  task :start, :roles => :app do
    invoke_command "/usr/local/bin/mongrel_rails start -c #{application_path} -p #{mongrel_port} -d -e #{rails_env} -a 127.0.0.1 -P #{home}/var/run/mongrel-#{application}-#{mongrel_port}.pid"
  end

  task :restart, :roles => :app do
    invoke_command "/usr/local/bin/mongrel_rails restart -P #{home}/var/run/mongrel-#{application}-#{mongrel_port}.pid"
  end

  task :stop, :roles => :app do
    invoke_command "/usr/local/bin/mongrel_rails stop -P #{home}/var/run/mongrel-#{application}-#{mongrel_port}.pid"
  end
end