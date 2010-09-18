Item.create!(:name => 'Home', :description => 'Home', :controller => 'homes', :action => 'show', :position => 0)
Item.create!(:name => 'Timer', :description => 'Timer', :controller => 'times', :action => 'index', :position => 1)
Item.create!(:name => 'Competitions', :description => 'Competitions', :controller => 'competitions', :action => 'index', :position => 2)
Item.create!(:name => 'Users', :description => 'Users', :controller => 'users', :action => 'index', :position => 3)
Item.create!(:name => 'Records', :description => 'Records', :controller => 'records', :action => 'index', :position => 4)

k = Kind.create!(:name => 'speed')
Kind.create!(:name => 'blindfolded')
Kind.create!(:name => 'one-handed')

Puzzle.create!(:name => '3x3x3', :kind => k, :scramble_length => 25, :attempt_count => 5, :average_format => 'average')