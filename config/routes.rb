ActionController::Routing::Routes.draw do |map|
  map.root :controller => 'clocks'
  
  map.resources :clocks
  map.resources :items
  map.resources :kinds
  map.resources :puzzles
  map.resources :users
  map.resources :sessions
  
  map.login 'login', :controller => 'sessions', :action => 'new'
  map.logout 'logout', :controller => 'sessions', :action => 'destroy'
end