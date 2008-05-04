class AveragesController < ApplicationController
  login :only => :index
  permit :moderator, :only => :destroy

  def index
    @user = User.find params[:user_id]
    @averages = @user.averages.for params[:puzzle_id]
    @puzzle = Puzzle.find params[:puzzle_id]
  end
  
  def destroy
    @average = Average.find params[:id]
    @average.destroy
    redirect_to user_kind_puzzle_averages_path
  end
end