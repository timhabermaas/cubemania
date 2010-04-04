Cubemania::Application.routes.draw do
  root :to => 'homes#show'

  resource :home, :only => :show
  resources :posts, :has_many => :comments
  
  resources :users do
    resources :puzzles, :has_many => :averages
  end
  
  resources :matches, :only => :index

  resources :puzzles do
    resources :times, :controller => :clocks
    resources :averages
    resources :matches do
      resources :times, :controller => :clocks
    end
    resources :users, :as => 'opponents' do
      resources :matches
    end
    resources :competitions do
      resources :times, :controller => :clocks
      resources :shouts
    end
    resources :scrambles
    #match 'competitions/:id/:date' => 'competitions#show', :as => 'competition_date'
    #match 'records/:type' => 'records#index', :as => 'records', :defaults => { :type => 'average' }, :type => /(single)|(average)/
  end
  match 'puzzles/:puzzle_id/records/:type' => 'records#index', :as => 'records', :defaults => { :type => 'average' }, :type => /(single)|(average)/

  resources :kinds
  resources :items # delete?
  
  resource :password_recovery
  
  resource :login
  match 'logout' => 'logins#destroy', :as => 'logout'
  match 'register' => 'users#new', :as => 'register'

  #map.connect '*path', :controller => 'errors', :action => 'not_found'
end