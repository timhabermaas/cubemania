namespace :records do
  desc 'Recalculate all single records'
  task :single => :environment do
    puzzles = Puzzle.all
    count = User.count
    User.find_each do |user|
      puts "User #{user.id}/#{count}: #{user.name}"
      puzzles.each do |puzzle|
        UpdateRecords.single user, puzzle
      end
    end
  end

  desc 'Recalculate all average of 5 records'
  task :avg5 => :environment do
    puzzles = Puzzle.all
    count = User.count
    User.find_each do |user|
      puts "User #{user.id}/#{count}: #{user.name}"
      puzzles.each do |puzzle|
        UpdateRecords.for_amount user, puzzle, 5
      end
    end
  end

  desc 'Recalculate all average of 12 records'
  task :avg12 => :environment do
    puzzles = Puzzle.all
    count = User.count
    User.find_each do |user|
      puts "User #{user.id}/#{count}: #{user.name}"
      puzzles.each do |puzzle|
        UpdateRecords.for_amount user, puzzle, 12
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