k = Kind.create!(:name => 'speed')
Kind.create!(:name => 'blindfolded')
Kind.create!(:name => 'one-handed')

Puzzle.create!(:name => '3x3x3', :kind => k, :scramble_length => 25, :attempt_count => 5, :average_format => 'average')