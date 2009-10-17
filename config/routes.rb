ActionController::Routing::Routes.draw do |map|
  map.root :controller => 'homes', :action => 'show'

  map.resource :home, :only => :show
  map.resources :posts, :has_many => :comments

  map.resources :puzzles, :except => :show do |puzzles|
    puzzles.resources :times, :controller => :clocks, :only => [:index, :create]
    puzzles.resources :averages
    puzzles.resources :matches
    puzzles.resources :competitions do |competitions|
      competitions.resources :times, :controller => :clocks, :only => [:index, :create]
      competitions.resources :shouts, :only => [:create, :destroy]
    end
    puzzles.resources :scrambles, :only => [:new, :index]
    puzzles.competition_date 'competitions/:id/:date', :controller => 'competitions', :action => 'show'
    puzzles.records 'records/:type', :controller => 'records', :defaults => { :type => 'average' }, :type => /(single)|(average)/
  end

  map.resources :users do |users|
    users.resources :puzzles, :has_many => :averages
  end

  map.resources :kinds, :exclude => :show
  map.resources :items # delete?
  
  map.resource :password_recovery
  
  map.resource :login, :only => [:show, :create, :destroy]
  map.logout 'logout', :controller => 'logins', :action => 'destroy'
  map.register 'register', :controller => 'users', :action => 'new'

  map.connect '*path', :controller => 'errors', :action => 'not_found'
end