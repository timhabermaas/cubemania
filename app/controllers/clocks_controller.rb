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
        average = update_record current_user.averages.record(@average.puzzle_id), @average
        single = update_record current_user.singles.record(@average.puzzle_id), @average.singles.sort_by(&:time).first
        flash[:notice] = 'You have a new personal average record!' if average
        flash[:notice] = 'You have a new personal single record!' if single
        flash[:notice] = 'You have a new personal single and average record!' if average and single
        @puzzle = @average.puzzle
      end
    end
  end

  private
    def update_record(old, new)
      if updated = ((old.nil? or new.time < old.time) and not new.dnf?)
        old.update_attribute :record, false unless old.nil?
        new.update_attribute :record, true
      end
      updated
    end
end