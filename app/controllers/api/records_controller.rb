module Api
  class RecordsController < ApiController
    def index
      puzzle = Puzzle.find params[:puzzle_id]
      user = User.find params[:user_id]

      @records = user.records.where(:puzzle_id => puzzle.id)

      respond_to do |format|
        format.html
        format.json
      end
    end
  end
end
