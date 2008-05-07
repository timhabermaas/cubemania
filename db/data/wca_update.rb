speed = Kind.find_by_name 'speed'
blindfolded = Kind.find_by_name 'blindfolded'
one_handed = Kind.find_by_name 'one-handed'
feet = Kind.find_by_name 'feet'

speed.puzzles.each do |puzzle|
  puzzle.update_attribute :attempt_count, 5
  puzzle.update_attribute :average_format, 'average'
end
speed.puzzles.find_by_name('clock').update_attribute :attempt_count, 3
speed.puzzles.find_by_name('clock').update_attribute :average_format, 'mean'
speed.puzzles.find_by_name('megaminx').update_attribute :attempt_count, 3
speed.puzzles.find_by_name('megaminx').update_attribute :average_format, 'mean'
speed.puzzles.find_by_name('square-1').update_attribute :attempt_count, 3
speed.puzzles.find_by_name('square-1').update_attribute :average_format, 'mean'

blindfolded.puzzles.each do |puzzle|
  puzzle.update_attribute :attempt_count, 3
  puzzle.update_attribute :average_format, 'best_of'
end

one_handed.puzzles.each do |puzzle|
  puzzle.update_attribute :attempt_count, 5
  puzzle.update_attribute :average_format, 'average'
end

feet.puzzles.each do |puzzle|
  puzzle.update_attribute :attempt_count, 5
  puzzle.update_attribute :average_format, 'average'
end