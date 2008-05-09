class AveragesController < ApplicationController
  login :except => []
  permit :moderator, :only => :destroy

  def index
    @user = User.find params[:user_id]
    @puzzle = Puzzle.find params[:puzzle_id]
    
    respond_to do |format|
      format.html { @averages = @user.averages.for params[:puzzle_id], true }
      format.xml { @averages = @user.averages.for params[:puzzle_id] }
      format.csv do
        @averages = @user.averages.for params[:puzzle_id], true
        if @averages.empty?
          flash[:notice] = 'You have no times yet. Use the timer!'
          redirect_to kind_puzzle_times_path
        end
        response.headers['Content-Type'] = 'text/csv; charset=iso-8859-1; header=present'
        response.headers['Content-Disposition'] = "attachment; filename=#{@puzzle.kind.name}_#{@puzzle.name}.csv"
      end
    end
  end
  
  def show
    @average = Average.find params[:id], :include => :singles
  end
  
  def destroy
    @average = Average.find params[:id]
    @average.destroy
    flash[:notice] = 'Successfully removed!'
    redirect_to user_kind_puzzle_averages_path
  end
end