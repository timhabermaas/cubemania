class RecordsController < ApplicationController
  def index
    params[:type] ||= "avg5"
    @puzzle = Puzzle.find params[:puzzle_id]
    @records =
      if params[:type] == "single"
        @puzzle.records.amount(1)
      elsif params[:type] == "avg12"
        @puzzle.records.amount(12)
      else
        @puzzle.records.amount(5)
      end.paginate(:page => params[:page], :per_page => 50)
  end

  def show
    @puzzle = Puzzle.find params[:puzzle_id]
    @record = @puzzle.records.find params[:id], :include => [:singles]
    @user = @record.user
  end
end