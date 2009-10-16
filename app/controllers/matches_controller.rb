class MatchesController < ApplicationController
  login :except => [:show, :index]
  
  def index
  end

  def show
  end

  def create
  end

  def new
    @user = User.find params[:user_id]
    @puzzle = Puzzle.find params[:puzzle_id]
  end
end
