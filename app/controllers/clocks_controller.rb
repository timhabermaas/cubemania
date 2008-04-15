class ClocksController < ApplicationController
  login :only => :index

  def index
    @puzzle = Puzzle.find params[:puzzle_id]
    respond_to &:html
  end

  def create
    @average = user.averages.build params[:average]
    @average.singles = params[:singles].map { |index, single| user.singles.build single }

    if @average.save
      update_record user.averages.record(@average.puzzle_id), @average
      update_record user.singles.record(@average.puzzle_id), @average.singles.sort_by(&:time).first
      respond_to { |format| format.js { @puzzle = @average.puzzle } }
    end
  end

  private
    def update_record(old, new)
      if old.nil? or new.time < old.time
        old.update_attribute :record, false unless old.nil?
        new.record = true
      end
    end
end