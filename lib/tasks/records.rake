namespace :records do
  def stats(user)
    @max_single_count ||= Single.count
    @max_user_count ||= User.count
    @single_count = (@single_count || 0) + user.singles_count

    puts "Singles #{@single_count}/#{@max_single_count}"
    puts "User #{user.id}/#{@max_user_count}: #{user.name}"
  end

  desc 'Recalculate all single records'
  task :single => :environment do
    puzzles = Puzzle.all
    type = RecordType.by_count(1)
    User.find_each do |user|
      stats user
      puzzles.each do |puzzle|
        singles = user.singles.where(:puzzle_id => puzzle.id).order("created_at")
        RecalculateRecordsHistory.for!(type, singles)
      end
    end
  end

  desc 'Recalculate all average of 5 records'
  task :avg5 => :environment do
    puzzles = Puzzle.all
    type = RecordType.by_count(5)
    User.find_each do |user|
      stats user
      puzzles.each do |puzzle|
        singles = user.singles.where(:puzzle_id => puzzle.id).order("created_at")
        RecalculateRecordsHistory.for!(type, singles)
      end
    end
  end

  desc 'Recalculate all average of 12 records'
  task :avg12 => :environment do
    puzzles = Puzzle.all
    type = RecordType.by_count(12)
    User.find_each do |user|
      stats user
      puzzles.each do |puzzle|
        singles = user.singles.where(:puzzle_id => puzzle.id).order("created_at")
        RecalculateRecordsHistory.for!(type, singles)
      end
    end
  end

  desc 'Recalculate all mean of 100 records'
  task :mean100 => :environment do
    puzzles = Puzzle.all
    type = RecordType.by_count(100)
    User.find_each do |user|
      stats user
      puzzles.each do |puzzle|
        singles = user.singles.where(:puzzle_id => puzzle.id).order("created_at")
        RecalculateRecordsHistory.for!(type, singles)
      end
    end
  end

  desc 'Recalculate all records (single, avg5 and avg12)'
  task :all do
    Rake::Task["records:single"].invoke
    Rake::Task["records:avg5"].invoke
    Rake::Task["records:avg12"].invoke
    Rake::Task["records:mean100"].invoke
  end
end