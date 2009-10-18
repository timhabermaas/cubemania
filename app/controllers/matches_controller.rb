class MatchesController < ApplicationController
  
  def index
  end

  def show
    @match = Match.find params[:id]
  end

  def create
    @match = current_user.matches.build(:puzzle_id => params[:puzzle_id], :opponent_id => params[:user_id])
    @match.save
  end

  def new
    @user = User.find params[:user_id]
    @puzzle = Puzzle.find params[:puzzle_id]
  end
end
