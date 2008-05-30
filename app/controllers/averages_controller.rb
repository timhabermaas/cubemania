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
        response.headers['Content-Type'] = 'text/csv; charset=iso-8859-1; header=present'
        response.headers['Content-Disposition'] = 'attachment; filename="' + @puzzle.kind.name + '_' + @puzzle.name + '.csv"'
      end
    end
  end
  
  def show
    @average = object true
  end
  
  def destroy
    object.destroy
    flash[:notice] = 'Successfully removed!'
    redirect_to user_puzzle_averages_path
  end
  
  private
    def object(singles = false)
      if singles
        Average.find params[:id], :include => :singles
      else
        Average.find params[:id]
      end
    end
end