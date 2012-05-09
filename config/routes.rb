begin
  default_puzzle = Puzzle.default.id
rescue # if there's no database yet, it shouldn't crash creating one
  default_puzzle = 1
end

Cubemania::Application.routes.draw do
  root :to => 'homes#show'

  match "/delayed_job" => DelayedJobWeb, :anchor => false

  resources :posts do
    resources :comments
  end

  resources :users
  resources :profiles

  resources :puzzles, :defaults => { :puzzle_id => default_puzzle } do
    resources :timers, :path => "timer"

    resources :competitions do
      member do
        post :compete
        get "/:date" => "competitions#show", :as => "date"
      end
      resources :shouts
    end

    resources :singles

    resources :records do
      post :share, :on => :member
    end
  end

  resources :kinds

  resources :authorizations
  match '/auth/facebook/callback' => 'authorizations#create'
  match '/auth/developer/callback' => 'authorizations#create'
  match '/auth/failure' => 'authorizations#failure'

  resource :reset_password

  resource :session
  match 'login' => 'sessions#new', :as => 'login'
  match 'logout' => 'sessions#destroy', :as => 'logout'
  match 'register' => 'users#new', :as => 'register'
end