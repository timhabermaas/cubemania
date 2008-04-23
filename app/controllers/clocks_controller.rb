class ClocksController < ApplicationController
  login :except => []

  def index
    @puzzle = Puzzle.find params[:puzzle_id]
  end
  
  def create
    params[:average][:puzzle_id] = params[:puzzle_id]
    @average = current_user.averages.build params[:average]
    @average.singles = params[:singles].map { |index, single| single[:puzzle_id] = params[:puzzle_id]; current_user.singles.build single }

    Average.transaction do
      if @average.save
        flash[:notice] = 'You have a new personal average record!' if @average.record
        flash[:notice] = 'You have a new personal single record!' unless @average.singles.select(&:record).empty?
        flash[:notice] = 'You have a new personal single and average record!' if @average.record and not @average.singles.select(&:record).empty?
        @puzzle = @average.puzzle
      end
    end
  end
end