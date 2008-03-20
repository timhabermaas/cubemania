ActionController::Routing::Routes.draw do |map|
  map.root :controller => 'puzzles'
  
  map.resources :items
  map.resources :kinds
  map.resources :puzzles
end
