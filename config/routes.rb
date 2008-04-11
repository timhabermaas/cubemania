ActionController::Routing::Routes.draw do |map|
  map.root :controller => 'clocks'
  
  map.resources :clocks, :collection => { :auto_complete_for_user_name => :get }
  map.resources :competitions
  map.resources :users, :has_many => :clocks
  map.resources :items
  map.resources :kinds
  map.resources :puzzles
  map.resource :login
  map.resources :singles
  map.resources :averages
  
  map.logout 'logout', :controller => 'logins', :action => 'destroy'
  
  map.connect 'records/:type', :controller => 'records', :defaults => { :type => 'single' }, :type => /(single)|(average)/
end