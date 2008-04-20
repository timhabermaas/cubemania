ActionController::Routing::Routes.draw do |map|
  map.root :controller => 'posts'

  map.resources :posts, :has_many => :comments
  map.resources :kinds do |kinds|
    kinds.resources :puzzles do |puzzles|
      puzzles.resources :clocks, :singles, :averages
      puzzles.records 'records/:type', :controller => 'records', :defaults => { :type => 'single' }, :type => /(single)|(average)/
    end
  end
  map.resources :competitions
  map.resources :users
  map.resources :items

  map.resource :login
  map.logout 'logout', :controller => 'logins', :action => 'destroy'
  map.register 'register', :controller => 'users', :action => 'new'
  
  map.connect '*path', :controller => 'errors', :action => 'not_found'
end