class RecordsController < ApplicationController
  def index
    @puzzle = Puzzle.find params[:puzzle_id]
  end
end