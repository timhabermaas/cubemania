desc 'Deploy current git branch to heroku'
task :deploy do
  current_branch = `git branch | grep '* '`[2..-1].chomp
  `git checkout heroku; git merge #{current_branch} && git push heroku heroku:master; git checkout #{current_branch}`
end
