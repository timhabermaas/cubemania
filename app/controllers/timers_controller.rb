class TimersController < ApplicationController
  login :except => []
  skip_load_and_authorize_resource
  load_and_authorize_resource :class => Single
  before_filter :load_puzzle

  def index
    @singles = current_user.singles.for(@puzzle).last_24_hours
  end

private
  def load_puzzle
    @puzzle = Puzzle.find params[:puzzle_id]
  end
end
