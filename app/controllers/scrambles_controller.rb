class ScramblesController < ApplicationController
  login :except => []
  
  def new
    @puzzle = Puzzle.find params[:puzzle_id]
    @scramble = @puzzle.scramble
    respond_to do |format|
      format.json { render :json => @scramble.to_json }
    end
  end
  
  def index
    @puzzle = Puzzle.find params[:puzzle_id]
    @scrambles = @puzzle.scrambles
    respond_to do |format|
      format.json #{ render :json => @scrambles, :include => @puzzle }
    end
  end
end
