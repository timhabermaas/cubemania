class ClocksController < ApplicationController
  login :except => []

  def index
    @puzzle = Puzzle.find params[:puzzle_id]
  end
  
  def create
    @average = current_user.averages.build params[:average].merge(:puzzle_id => params[:puzzle_id])
    @average.singles = params[:singles].map { |index, single| current_user.singles.build single.merge(:puzzle_id => params[:puzzle_id]) }.sort_by(&:time)

    if @average.save
      flash[:notice] = @average.notice
      @puzzle = @average.puzzle
    end
  end
end