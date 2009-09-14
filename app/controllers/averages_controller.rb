class AveragesController < ApplicationController
  login :except => []
  permit :owner, :only => :destroy

  def index
    @user = User.find params[:user_id]
    @puzzle = Puzzle.find params[:puzzle_id]
    @averages = @user.averages.paginate_for params[:puzzle_id], params[:page]
    
    respond_to do |format|
      format.html
      format.xml
      format.json { render :json => {:averages => @averages, :user => @user} }
      format.csv do
        response.headers['Content-Type'] = 'text/csv; charset=iso-8859-1; header=present'
        response.headers['Content-Disposition'] = 'attachment; filename="' + @puzzle.kind.name + '_' + @puzzle.name + '.csv"'
      end
    end
  end
  
  def show
    object :include => :singles
  end
  
  def destroy
    object.destroy
    flash[:notice] = 'Successfully removed!'
    redirect_to user_puzzle_averages_path
  end
  
  private
    def object(options = nil)
      @average ||= Average.find params[:id], options
    end
end