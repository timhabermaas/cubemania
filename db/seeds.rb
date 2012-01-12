speed = Kind.create! :name => 'speed', :css_class => 'speed', :css_position => 0
Kind.create! :name => 'blindfolded', :css_class => 'bld', :css_position => 1
Kind.create! :name => 'one-handed', :css_class => 'oh', :css_position => 2

Puzzle.create! :name => '3x3x3',
               :kind => speed,
               :scramble_length => 25,
               :attempt_count => 5,
               :average_format => 'average',
               :css_class => "three", :css_position => 1