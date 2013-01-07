class TimersController < ApplicationController
  skip_load_and_authorize_resource

  def index
    @puzzle = Puzzle.find params[:puzzle_id]
  end
end
