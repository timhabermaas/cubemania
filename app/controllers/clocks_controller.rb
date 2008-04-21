class ClocksController < ApplicationController
  login :except => []

  def index
    @puzzle = Puzzle.find params[:puzzle_id]
    respond_to &:html
  end
  
  def create
    @average = current_user.averages.build params[:average]
    @average.singles = params[:singles].map { |index, single| current_user.singles.build single }

    if @average.save
      average = update_record current_user.averages.record(@average.puzzle_id), @average
      single = update_record current_user.singles.record(@average.puzzle_id), @average.singles.sort_by(&:time).first
      if average
        flash[:notice] = "You have a new personal average record!"
      end
      if single
        flash[:notice] = "You have a new personal single record!"
      end
      if average && single
        flash[:notice] = "You have a new personal single and average record!"
      end
      respond_to { |format| format.js { @puzzle = @average.puzzle } }
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