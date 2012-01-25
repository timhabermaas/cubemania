begin
  default_puzzle = Puzzle.default.id
rescue # if there's no database yet, it shouldn't crash creating one
  default_puzzle = 1
end

#require 'api'

Cubemania::Application.routes.draw do
  root :to => 'homes#show'

  match "/delayed_job" => DelayedJobWeb, :anchor => false

  #match '/api/*other' => CubemaniaAPI

  resources :posts do
    resources :comments
  end

  resources :users do
    resources :singles
  end

  # resources :matches, :only => :index

  resources :puzzles, :defaults => { :puzzle_id => default_puzzle } do
    resources :timers, :path => "timer" do
      put :dnf, :on => :member
      put :plus2, :on => :member
      get :more, :on => :collection
      get :chart, :on => :collection
    end
    resources :singles
    # resources :matches do
    #   resources :times, :controller => :clocks
    # end
    # resources :users do
    #   resources :matches, :defaults => { :puzzle_id => default_puzzle }
    # end
    resources :competitions do
      post :compete, :on => :member
      resources :shouts
    end
    resources :scrambles
    #match 'competitions/:id/:date' => 'competitions#show', :as => 'competition_date'
    #match 'records/:type' => 'records#index', :as => 'records', :defaults => { :type => 'average' }, :type => /(single)|(average)/
  end
  match 'puzzles/:puzzle_id/records/:type' => 'records#index', :as => 'puzzle_records', :defaults => { :type => 'avg5', :puzzle_id => default_puzzle },
          :type => /(single)|(avg5)|(avg12)/
  #match 'puzzles/:puzzle_id/timer' => 'times#index', :as => 'puzzle_times', :defaults => { :puzzle_id => default_puzzle }
  match 'puzzles/:puzzle_id/competitions/:id/:date' => 'competitions#show', :as => 'puzzle_competition_date'

  resources :kinds
  resources :items

  resources :authorizations
  match '/auth/twitter/callback' => 'authorizations#create'
  match '/auth/facebook/callback' => 'authorizations#create'
  match '/auth/developer/callback' => 'authorizations#create'
  match '/auth/failure' => 'authorizations#failure'

  resource :reset_password

  resource :session
  match 'login' => 'sessions#new', :as => 'login'
  match 'logout' => 'sessions#destroy', :as => 'logout'
  match 'register' => 'users#new', :as => 'register'
end