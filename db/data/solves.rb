tim = User.find_by_name 'tim'
simon = User.find_by_name 'simon'

# 2x2x2
20.times do
  tim.clocks.create :puzzle_id => 3, :scramble => Puzzle.find_by_id(3).scramble, :time => (7 + 4*rand)*100
end
# 3x3x3
42.times do
  tim.clocks.create :puzzle_id => 4, :scramble => Puzzle.find_by_id(4).scramble, :time => (15 + 6*rand)*100
end
# 4x4x4
10.times do
  tim.clocks.create :puzzle_id => 5, :scramble => Puzzle.find_by_id(5).scramble, :time => (80 + 30*rand)*100
end

# megaminx
30.times do
  simon.clocks.create :puzzle_id => 8, :scramble => Puzzle.find_by_id(8).scramble, :time => (70 + 20*rand)*100
end
# pyraminx
42.times do
  tim.clocks.create :puzzle_id => 10, :scramble => Puzzle.find_by_id(10).scramble, :time => (8 + 6*rand)*100
end