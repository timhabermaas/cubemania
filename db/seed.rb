Item.create!(:name => 'Home', :description => 'Home', :url => '/', :position => 0)
Item.create!(:name => 'Timer', :description => 'Timer', :url => '/puzzles/1/times', :position => 1)
Item.create!(:name => 'Competitions', :description => 'Competitions', :url => '/puzzles/1/competitions', :position => 2)
Item.create!(:name => 'Users', :description => 'Users', :url => '/users', :position => 3)
Item.create!(:name => 'Records', :description => 'Records', :url => '/puzzles/1/times', :position => 4)

k = Kind.create!(:name => 'speed')
Kind.create!(:name => 'blindfolded')
Kind.create!(:name => 'one-handed')

Puzzle.create!(:name => '3x3x3', :kind => k, :scramble_length => 25, :attempt_count => 5, :average_format => 'average')