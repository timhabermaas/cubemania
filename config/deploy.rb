set :application, 'cubemania.org'
set :user, 'simon'
set :svn_user, 'simonwacker'
set :svn_password, Proc.new { Capistrano::CLI.password_prompt('SVN Password: ') }
set :repository, Proc.new { "--username #{svn_user} --password #{svn_password} --no-auth-cache http://svn2.assembla.com/svn/cubemania/trunk" }
set :deploy_to, "/srv/#{application}"
set :ip, '209.20.70.85'

role :app, ip
role :web, ip
role :db, ip, :primary => true

namespace :deploy do
  task :start do
    sudo 'thin start -C /etc/thin/cubemania.org.yml'
  end
  task :stop do 
    sudo 'thin stop -C /etc/thin/cubemania.org.yml'
  end
  task :restart do 
    sudo 'thin restart -C /etc/thin/cubemania.org.yml'
  end
end