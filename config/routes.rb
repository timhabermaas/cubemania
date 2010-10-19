DEFAULT_PUZZLE = Puzzle.default

Cubemania::Application.routes.draw do
  root :to => 'homes#show'

  resources :posts do
    resources :comments
  end

  resources :users do
    resources :puzzles do
      resources :singles
    end
  end

  # resources :matches, :only => :index

  resources :puzzles, :defaults => { :id => DEFAULT_PUZZLE } do
    resources :times
    resources :singles
    # resources :matches do
    #   resources :times, :controller => :clocks
    # end
    resources :users do
      # resources :matches, :defaults => { :puzzle_id => DEFAULT_PUZZLE }
    end
    resources :competitions do
      resources :times
      resources :shouts
    end
    resources :scrambles
    #match 'competitions/:id/:date' => 'competitions#show', :as => 'competition_date'
    #match 'records/:type' => 'records#index', :as => 'records', :defaults => { :type => 'average' }, :type => /(single)|(average)/
  end
  match 'puzzles/:puzzle_id/records/:type' => 'records#index', :as => 'puzzle_records', :defaults => { :type => 'average', :puzzle_id => DEFAULT_PUZZLE },
          :type => /(single)|(average)/
  match 'puzzles/:puzzle_id/times' => 'times#index', :as => 'puzzle_times', :defaults => { :puzzle_id => DEFAULT_PUZZLE }
  match 'puzzles/:puzzle_id/competitions' => 'competitions#index', :as => 'puzzle_competitions', :defaults => { :puzzle_id => DEFAULT_PUZZLE }
  match 'puzzles/:puzzle_id/competitions/:id/:date' => 'competitions#show', :as => 'puzzle_competition_date'

  resources :kinds
  resources :items

  resource :password_recovery

  resource :login
  match 'logout' => 'logins#destroy', :as => 'logout'
  match 'register' => 'users#new', :as => 'register'

  #map.connect '*path', :controller => 'errors', :action => 'not_found'
end