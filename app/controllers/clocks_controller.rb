class ClocksController < ApplicationController
  login :except => []

  def index
    @puzzle = Puzzle.find params[:puzzle_id]
    respond_to &:html
  end
  
  def create
    @average = user.averages.build params[:average]
    @average.singles = params[:singles].map { |index, single| user.singles.build single }

    if @average.save
      average = update_record user.averages.record(@average.puzzle_id), @average
      single = update_record user.singles.record(@average.puzzle_id), @average.singles.sort_by(&:time).first
      # sorry simon, i've got no freaking idea what's wrong with my if else statements.
      if average
        flash[:notice] = "You got a new personal average record!"
      end
      if single
        flash[:notice] = "You got a new personal single record!"
      end
      if average && single
        flash[:notice] = "You got a new personal single and average record!"
      end
      # What about a "You are ranked 21. with this average" message?
      respond_to { |format| format.js { @puzzle = @average.puzzle } }
    end
  end

  private
    def update_record(old, new)
      updated = false
      if (old.nil? or new.time < old.time) and not new.dnf?
        old.update_attribute :record, false unless old.nil?
        new.update_attribute :record, true
        updated = true
      end
      updated
    end
end