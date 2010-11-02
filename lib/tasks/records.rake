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
    puzzles = Puzzle.all
    count = User.count
    User.find_each do |user|
      p "User #{user.id}/#{count}: #{user.name}"
      puzzles.each do |puzzle|
        user.calculate_record! puzzle.id, 5
      end
    end
  end

  desc 'Recalculate all average of 5 records'
  task :avg12 => :environment do
    puzzles = Puzzle.all
    count = User.count
    User.find_each do |user|
      p "User #{user.id}/#{count}: #{user.name}"
      puzzles.each do |puzzle|
        user.calculate_record! puzzle.id, 12
      end
    end
  end

  desc 'Recalculate all records (single, avg5 and avg12)'
  task :all do
    Rake::Task["records:single"].invoke
    Rake::Task["records:avg5"].invoke
    Rake::Task["records:avg12"].invoke
  end

end