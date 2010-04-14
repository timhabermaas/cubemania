class AveragesController < ApplicationController
  login :except => []
  permit :owner, :only => :destroy

  def index
    @user = User.find params[:user_id]
    @puzzle = Puzzle.find params[:puzzle_id]
    @averages = @user.averages.where(:puzzle_id => params[:puzzle_id]).order('created_at desc').includes(:singles)
    
    respond_to do |format|
      format.html
      format.xml
      format.json
      format.csv do
        response.headers['Content-Type'] = 'text/csv; charset=iso-8859-1; header=present'
        response.headers['Content-Disposition'] = 'attachment; filename="' + @puzzle.kind.name + '_' + @puzzle.name + '.csv"'
      end
    end
  end
  
  def show
    @user = current_user
    @puzzle = Puzzle.find params[:puzzle_id]
    object :include => :singles
  end
  
  def destroy
    if object.destroy
      flash[:notice] = 'Successfully removed!'
    else
      flash[:notice] = "You can't remove this average, because it's part of a match"
    end
    redirect_to user_puzzle_averages_path
  end
  
  private
    def object(options = nil)
      @average ||= Average.find params[:id], options
    end
end