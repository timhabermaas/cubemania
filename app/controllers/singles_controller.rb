class SinglesController < ApplicationController
  respond_to :json

  def index
    user = User.find params[:user_id]
    puzzle = Puzzle.find params[:puzzle_id]
    respond_with user.singles.for(puzzle).limit(150)
  end

  def create
    puzzle = Puzzle.find params[:puzzle_id]
    single = current_user.singles.build(params[:single].merge(:puzzle_id => puzzle.id))
    if single.save
      UpdateRecentRecords.for(current_user, puzzle)
    end
    respond_with single
  end

  def destroy
    single = current_user.singles.find params[:id]
    single.destroy
    respond_with single
  end

  def update
    single = current_user.singles.find params[:id]
    single.update_attributes params[:single]
    respond_with single
  end
end
