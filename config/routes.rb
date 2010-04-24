Cubemania::Application.routes.draw do |map|
  root :to => 'homes#show'

  resource :facebook do
    get :callback
  end

  resources :posts do
    resources :comments
  end

  resources :users do
    resources :puzzles do
      resources :averages
    end
  end

  resources :matches, :only => :index

  resources :puzzles, :defaults => {:id => DEFAULT_PUZZLE} do
    resources :times, :controller => :clocks
    resources :averages do
      post :tweet, :on => :member
    end
    resources :matches do
      resources :times, :controller => :clocks
    end
    resources :users do
      resources :matches, :defaults => { :puzzle_id => DEFAULT_PUZZLE }
    end
    resources :competitions do
      resources :times, :controller => :clocks
      resources :shouts
    end
    resources :scrambles
    #match 'competitions/:id/:date' => 'competitions#show', :as => 'competition_date'
    #match 'records/:type' => 'records#index', :as => 'records', :defaults => { :type => 'average' }, :type => /(single)|(average)/
  end
  match 'puzzles/:puzzle_id/records/:type' => 'records#index', :as => 'puzzle_records', :defaults => { :type => 'average', :puzzle_id => DEFAULT_PUZZLE },
          :type => /(single)|(average)/
  match 'puzzles/:puzzle_id/times' => 'clocks#index', :as => 'puzzle_times', :defaults => { :puzzle_id => DEFAULT_PUZZLE }
  match 'puzzles/:puzzle_id/competitions' => 'competitions#index', :as => 'puzzle_competitions', :defaults => { :puzzle_id => DEFAULT_PUZZLE }
  match 'puzzles/:puzzle_id/competitions/:id/:date' => 'competitions#show', :as => 'puzzle_competition_date'

  resources :kinds
  resources :items

  resource :password_recovery

  resource :login
  match 'logout' => 'logins#destroy', :as => 'logout'
  match 'register' => 'users#new', :as => 'register'

  map.connect '*path', :controller => 'errors', :action => 'not_found'
end