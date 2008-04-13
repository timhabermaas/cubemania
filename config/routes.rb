ActionController::Routing::Routes.draw do |map|
  map.resources :comments

  map.root :controller => 'posts'
  
  map.resources :posts
  map.resources :clocks, :singles, :averages
  map.resources :competitions
  map.records 'records/:type', :controller => 'records', :defaults => { :type => 'single' }, :type => /(single)|(average)/
  map.resources :users, :has_many => :clocks
  map.resources :puzzles, :kinds
  map.resources :items
  
  map.resource :login
  map.logout 'logout', :controller => 'logins', :action => 'destroy'
end