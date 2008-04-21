class ClocksController < ApplicationController
  login :except => []

  def index
    @puzzle = Puzzle.find params[:puzzle_id]
  end
  
  def create
    @average = current_user.averages.build params[:average]
    @average.singles = params[:singles].map { |index, single| current_user.singles.build single }

    if @average.save
      #records should be updated within the same transaction, i.e. in after_save callback in model Average
      average = update_record current_user.averages.record(@average.puzzle_id), @average
      single = update_record current_user.singles.record(@average.puzzle_id), @average.singles.sort_by(&:time).first
      flash[:notice] = 'You have a new personal average record!' if average
      flash[:notice] = 'You have a new personal single record!' if single
      flash[:notice] = 'You have a new personal single and average record!' if average and single
      @puzzle = @average.puzzle
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