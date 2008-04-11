class RecordsController < ApplicationController
  def index
    if params[:puzzle].blank?
      @kinds = Kind.find :all
    else
      @puzzle = Puzzle.find params[:puzzle]
      render :action => 'show'
    end
  end
end