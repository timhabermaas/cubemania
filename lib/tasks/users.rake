namespace :users do
  desc 'Calculate wasted_time'
  task :calculate_wasted_time => :environment do
    count = User.count
    User.find_each do |user|
      puts "User #{user.id}/#{count}: #{user.name}"
      user.update_attribute :wasted_time, user.singles.not_dnf.sum("singles.time") / 1000
    end
  end
end
