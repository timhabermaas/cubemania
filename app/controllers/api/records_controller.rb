module Api
  class RecordsController < ApiController
    respond_to :json

    def index
      puzzle = Puzzle.find params[:puzzle_id]
      user = User.find params[:user_id]

      @records = user.records.where(:puzzle_id => puzzle.id)
      respond_with :api, puzzle, @records
    end
  end
end
