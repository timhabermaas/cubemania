speed = Kind.create! :name => 'speed', :short_name => '', :css_position => 0
Kind.create! :name => 'blindfolded', :short_name => 'BLD', :css_position => 1
Kind.create! :name => 'one-handed', :short_name => 'OH', :css_position => 2

Puzzle.create! :name => '3x3x3',
               :kind => speed,
               :scramble_length => 25,
               :attempt_count => 5,
               :average_format => 'average',
               :css_position => 1