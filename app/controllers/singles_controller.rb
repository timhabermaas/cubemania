class SinglesController < ApplicationController
  respond_to :html, :json

  def index
    user = User.find params[:user_id]
    puzzle = Puzzle.find params[:puzzle_id]
    @singles = user.singles.where(:puzzle_id => params[:puzzle_id])
    @singles = @singles.group_by do |s|
      [s.created_at.year, s.created_at.strftime('%W').to_i]
    end.map do |date, s|
      [((date.first - 1970)*356*24*60*60 + date.last*7*24*60*60) * 1000, s.map(&:time).inject(:+) / s.size.to_f]
    end
    respond_to do |format|
      format.html
      format.json do
        render :json => @singles.to_json(:only => :time)
      end
    end
  end
end
