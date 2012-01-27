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

  resources :users

  resources :puzzles, :defaults => { :puzzle_id => default_puzzle } do
    resources :timers, :path => "timer" do
      put :dnf, :on => :member
      put :plus2, :on => :member
      get :more, :on => :collection
      get :chart, :on => :collection
    end

    resources :competitions do
      post :compete, :on => :member
      resources :shouts
    end
    resources :scrambles

    resources :records
    #match 'competitions/:id/:date' => 'competitions#show', :as => 'competition_date'
  end

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