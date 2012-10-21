begin
  default_puzzle = Puzzle.default.slug
rescue # if there's no database yet, it shouldn't crash creating one
  default_puzzle = 1
end

Cubemania::Application.routes.draw do
  root :to => 'homes#show'

  match "/delayed_job" => DelayedJobWeb, :anchor => false

  namespace :api do
    resources :users do
      post "block", :on => :member
    end
    resources :puzzles do
      resources :singles
      resources :records
    end
  end

  resources :posts do
    resources :comments, :only => [:create, :destroy]
  end

  resources :profiles

  resources :puzzles, :defaults => { :puzzle_id => default_puzzle } do
    resources :records, :only => [:show] do
      get :share, :on => :member
    end
  end

  resources :kinds

  resource :reset_password, :only => [:new, :create]

  resource :session
  match 'login' => 'sessions#new', :as => 'login'
  match 'logout' => 'sessions#destroy', :as => 'logout'
  match 'register' => 'profiles#new', :as => 'register'

  match '*path' => 'backbones#show'
end
