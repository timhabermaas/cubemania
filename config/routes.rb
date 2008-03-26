ActionController::Routing::Routes.draw do |map|
  map.root :controller => 'clocks'
  
  map.resources :clocks
  map.resources :competitions
  map.resources :users, :has_many => :clocks
  map.resources :records
  map.resources :items
  map.resources :kinds
  map.resources :puzzles
  map.resource :login
  
  map.logout 'logout', :controller => 'logins', :action => 'destroy'
end