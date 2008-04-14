ActionController::Routing::Routes.draw do |map|
  map.resources :comments

  map.root :controller => 'posts'
  
  map.resources :posts, :has_many => :comments
  map.resources :singles, :averages
  map.resources :competitions
  map.resources :users, :has_many => :clocks
  map.resources :kinds do |kinds|
    kinds.resources :puzzles do |puzzles|
      puzzles.resources :clocks
      puzzles.records 'records/:type', :controller => 'records', :defaults => { :type => 'single' }, :type => /(single)|(average)/
    end
  end
  map.resources :items
  
  map.resource :login
  map.logout 'logout', :controller => 'logins', :action => 'destroy'
  map.register 'register', :controller => 'users', :action => 'new'
end