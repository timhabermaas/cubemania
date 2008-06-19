class RecordsController < ApplicationController
  def index
    @puzzle = Puzzle.find params[:puzzle_id]
    @records = @puzzle.records.send params[:type], params[:page]
  end
end