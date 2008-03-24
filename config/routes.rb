ActionController::Routing::Routes.draw do |map|
  map.root :controller => 'clocks'
  
  map.resources :clocks
  map.resources :items
  map.resources :kinds
  map.resources :puzzles
  map.resources :users
  map.resource :login
  
  map.logout 'logout', :controller => 'logins', :action => 'destroy'
end