class SinglesController < ApplicationController
  respond_to :html, :json

  def index
    user = User.find params[:user_id]
    puzzle = Puzzle.find params[:puzzle_id]
    @singles = user.singles.for(puzzle).recent(150).select("time")
    respond_to do |format|
      format.html
      format.json do
        render :json => @singles
      end
    end
  end
end
