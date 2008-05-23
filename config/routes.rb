ActionController::Routing::Routes.draw do |map|
  map.root :controller => 'homes', :action => 'show'

  map.resource :home
  map.resources :posts, :has_many => :comments

  map.resources :puzzles do |puzzles|
    puzzles.resources :times, :controller => :clocks
    puzzles.resources :singles, :averages
    puzzles.resources :competitions
    puzzles.competition 'competitions/:id/:date', :controller => 'competitions', :action => 'show'
    puzzles.records 'records/:type', :controller => 'records', :defaults => { :type => 'average' }, :type => /(single)|(average)/
  end

  # don't break old URLs
  map.resources :kinds do |kinds|
    kinds.resources :puzzles do |puzzles|
      puzzles.resources :times, :controller => :clocks
      puzzles.resources :singles, :averages
      puzzles.records 'records/:type', :controller => 'records', :defaults => { :type => 'average' }, :type => /(single)|(average)/
    end
  end

  map.resources :users do |users|
    users.resources :puzzles, :has_many => [:singles, :averages]
    # don't break old URLs
    users.resources :kinds do |kinds|
      users.resources :puzzles, :has_many => [:singles, :averages]
    end
  end

  map.resources :kinds
  map.resources :items

  map.resource :login
  map.logout 'logout', :controller => 'logins', :action => 'destroy'
  map.register 'register', :controller => 'users', :action => 'new'

  map.connect '*path', :controller => 'errors', :action => 'not_found'
end