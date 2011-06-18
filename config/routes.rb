begin
  DEFAULT_PUZZLE = Puzzle.default
rescue # if there's no database yet, it shouldn't crash creating one
  DEFAULT_PUZZLE = 1
end

require 'api'

Cubemania::Application.routes.draw do
  root :to => 'homes#show'

  match '/api/*other' => CubemaniaAPI

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
    resources :times do
      put :dnf, :on => :member
      put :plus2, :on => :member
    end
    resources :singles
    # resources :matches do
    #   resources :times, :controller => :clocks
    # end
    # resources :users do
    #   resources :matches, :defaults => { :puzzle_id => DEFAULT_PUZZLE }
    # end
    resources :competitions do
      resources :times
      resources :shouts
    end
    resources :scrambles
    #match 'competitions/:id/:date' => 'competitions#show', :as => 'competition_date'
    #match 'records/:type' => 'records#index', :as => 'records', :defaults => { :type => 'average' }, :type => /(single)|(average)/
  end
  match 'puzzles/:puzzle_id/records/:type' => 'records#index', :as => 'puzzle_records', :defaults => { :type => 'avg5', :puzzle_id => DEFAULT_PUZZLE },
          :type => /(single)|(avg5)|(avg12)/
  match 'puzzles/:puzzle_id/times' => 'times#index', :as => 'puzzle_times', :defaults => { :puzzle_id => DEFAULT_PUZZLE }
  match 'puzzles/:puzzle_id/competitions' => 'competitions#index', :as => 'puzzle_competitions', :defaults => { :puzzle_id => DEFAULT_PUZZLE }
  match 'puzzles/:puzzle_id/competitions/:id/:date' => 'competitions#show', :as => 'puzzle_competition_date'

  match 'auth/:provider/callback', :to => 'facebooks#create'

  resources :kinds
  resources :items

  resource :password_recovery

  resource :login
  match 'logout' => 'logins#destroy', :as => 'logout'
  match 'register' => 'users#new', :as => 'register'
end