class RecordsController < ApplicationController
  def index
    params[:puzzle] ||= '4'
    params[:kind] ||= '2'
    @puzzle = Puzzle.find params[:puzzle]
  end
end