namespace :records do
  desc 'Recalculate all single records'
  task :single => :environment do
=begin
    User.find_each do |user|
      p "User: #{user.name}, id: #{user.id}, singles: #{user.singles_count}"
      user.singles.select("distinct(singles.puzzle_id)").each do |puzzle|
        time = user.singles.best(puzzle.puzzle_id).try(:time)
        unless time == nil
          record = Record.find_or_create_by_puzzle_id_and_user_id(puzzle.puzzle_id, user.id, :time => time, :amount => 1, :singles => [])
        end
      end
    end
  end
=end
    Single.select('id, MIN(time) as time, user_id, puzzle_id').where(:dnf => false).group('user_id, puzzle_id').find_each do |record|
      r = Record.find_or_create_by_puzzle_id_and_user_id(record.puzzle_id, record.user_id, :time => record.time, :amount => 1, :singles => [])
      p "user: #{record.user_id}"
    end
  end
end