class RecordsController < ApplicationController
  def index
    @puzzle = Puzzle.find params[:puzzle_id]
    @records = 
      if params[:type] == "single"
        @puzzle.single_records
      else
        @puzzle.average_records
      end
  end
end