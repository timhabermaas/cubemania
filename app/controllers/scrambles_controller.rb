class ScramblesController < ApplicationController
  login :only => []
  
  def new
    @puzzle = Puzzle.find params[:puzzle_id]
    @scramble = @puzzle.scramble
    respond_to do |format|
      format.json { render :json => @scramble.to_json }
    end
  end
end
