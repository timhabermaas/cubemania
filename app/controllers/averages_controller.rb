class AveragesController < ApplicationController
  login :except => []

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

  def destroy
    if object.destroy
      respond_to do |format|
        format.html { flash[:notice] = 'Successfully removed!'; redirect_to user_puzzle_averages_path }
        format.js
      end
    end
  end

  def show
    @user = User.find params[:user_id]
    @puzzle = Puzzle.find params[:puzzle_id]
    object :include => :singles
    respond_to do |format|
      format.js
    end
  end

  private
    def object(options = nil)
      @average ||= Average.find params[:id], options
    end
end