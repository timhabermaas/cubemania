begin
  default_puzzle = Puzzle.default.slug
rescue # if there's no database yet, it shouldn't crash creating one
  default_puzzle = 1
end

Cubemania::Application.routes.draw do
  root :to => 'homes#show'

  namespace :api do
    resources :users
    resources :puzzles do
      resources :singles do
        get :chart, :on => :collection
      end
      resources :records
    end
  end

  resources :activities, :only => [:index, :show] do
    resources :comments, :only => [:create, :destroy]
  end
  match 'feed' => "activities#index"

  resources :posts do
    resources :comments, :only => [:create, :destroy]
  end

  resources :users do
    post :block, :on => :member
    post :follow, :on => :member
    post :unfollow, :on => :member
    resources :records, :only => [:show] do
      get :share, :on => :member
    end
  end

  resources :puzzles, :defaults => { :puzzle_id => default_puzzle } do
    resources :records, :only => [:index]

    resources :timers, :path => "timer"
  end

  resources :kinds

  resource :reset_password, :only => [:new, :create]

  resource :session
  match 'login' => 'sessions#new', :as => 'login'
  match 'logout' => 'sessions#destroy', :as => 'logout'
  match 'register' => 'users#new', :as => 'register'

  match '*path' => "errors#not_found"
end
