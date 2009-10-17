class MatchesController < ApplicationController
  login :except => [:show, :index]
  
  def index
  end

  def show
    @match = Match.find params[:id]
  end

  def create
    @match = current_user.matches.build(params[:match].merge(:puzzle_id => params[:puzzle_id]))
    if @match.save
      redirect_to puzzle_match_path(params[:puzzle_id], @match)
    else
      flash[:notice] = @match.errors.full_messages.join('<br />')
      redirect_to new_puzzle_match_path(params[:puzzle_id], :opponent_id => params[:user_id])
    end
  end

  def new
    @user = User.find params[:opponent_id]
    @puzzle = Puzzle.find params[:puzzle_id]
  end
end
