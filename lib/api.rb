class CubemaniaAPI < Grape::API
  prefix 'api'
  version 'v1'

  helpers do
    def current_user
      auth = Rack::Auth::Basic::Request.new(request.env)
      if auth.provided? && auth.basic? && auth.credentials
        @current_user ||= User.authorize(*auth.credentials)
      else
        nil
      end
    end

    def authorize!
      error!('401 Unauthorized', 401) unless current_user
    end
  end

  resources :users do
    get { User.all }
  end

  namespace '/puzzles/:puzzle_id/singles' do
    get do
      authorize!
      user = User.find params[:user_id]
      user.singles.for params[:puzzle_id]
    end

    post do
      authorize!
      current_user.singles.create params[:single]
    end
  end

  get '/puzzles/:puzzle_id/scrambles/:num' do
    num = params[:num].to_i > 200 ? 200 : params[:num].to_i
    puzzle = Puzzle.find params[:puzzle_id]
    (1..num).to_a.map { puzzle.scramble }
  end
end