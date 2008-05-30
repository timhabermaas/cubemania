class ClocksController < ApplicationController
  login :except => []

  def index
    @puzzle = Puzzle.find params[:puzzle_id]
    @scrambles = @puzzle.all_scrambles
  end

  def create
    @average = current_user.averages.build params[:average].merge(:puzzle_id => params[:puzzle_id], :competition_id => params[:competition_id])
    @average.singles = params[:singles].map { |index, single| current_user.singles.build single.merge(:puzzle_id => params[:puzzle_id], :competition_id => params[:competition_id], :position => index) }

    if @average.save
      @puzzle = @average.puzzle
    end
    @scrambles = @puzzle.all_scrambles
  end
end