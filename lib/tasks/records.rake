namespace :records do
  desc 'Recalculate all single records'
  task :single => :environment do
    ActiveRecord::Base.record_timestamps = false
    puzzles = Puzzle.all
    count = User.count
    User.find_each do |user|
      p "User #{user.id}/#{count}: #{user.name}"
      puzzles.each do |puzzle|
        single = user.singles.best(puzzle.id)
        if single
          record = Record.find_by_puzzle_id_and_user_id_and_amount(puzzle.id, user.id, 1)
          if record
            record.update_attributes(:time => single.time, :created_at => single.created_at, :updated_at => single.created_at)
          else
            Record.create!(:puzzle_id => puzzle.id, :user_id => user.id,
                           :amount => 1, :time => single.time, :created_at => single.created_at,
                           :updated_at => single.updated_at)
          end
        end
      end
    end
  end

  desc 'Recalculate all average of 5 records'
  task :avg5 => :environment do
    ActiveRecord::Base.record_timestamps = false
    puzzles = Puzzle.all
    count = User.count
    User.find_each do |user|
      p "User #{user.id}/#{count}: #{user.name}"
      puzzles.each do |puzzle|
        ra = user.best_average(puzzle.id, 5)
        if ra
          timestamp = ra.singles.last.created_at
          record = Record.find_by_puzzle_id_and_user_id_and_amount(puzzle.id, user.id, 5)
          if record and ra.average
            record.update_attributes(:time => ra.average, :created_at => timestamp, :updated_at => timestamp)
          elsif record.nil? and ra.average
            Record.create!(:puzzle_id => puzzle.id, :user_id => user.id, :amount => 5,
                          :time => ra.average, :created_at => timestamp,
                          :updated_at => timestamp)
          elsif record and ra.average.nil?
            record.destroy
          end
        end
      end
    end
  end

  desc 'Recalculate all records (single, avg5 and avg12)'
  task :all => [:single, :avg5]

end