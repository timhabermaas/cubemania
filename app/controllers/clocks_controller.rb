class ClocksController < ApplicationController
  login

  def index
    params[:puzzle] ||= '4'
    params[:kind] ||= '2'
    
    respond_to do |format|
      format.html { @puzzle = Puzzle.find params[:puzzle] }
      format.xml { @clocks = User.find(params[:user_id]).averages.for params[:puzzle] }
    end
  end

  def create
    @average = user.averages.build params[:average]
    @average.singles = params[:singles].map { |index, single| user.singles.build single }

    update_record user.averages.record(@average.puzzle_id), @average
    update_record user.singles.record(@average.puzzle_id), @average.singles.sort_by(&:time).first

    @average.save!

    respond_to do |format|
      format.js { @puzzle = @average.puzzle }
    end
  end

  def auto_complete_for_user_name
    @users = User.find :all, :conditions => ["LOWER(name) LIKE ?", '%' + params[:val] + '%'], :order => "name ASC", :limit => 10
  end

  private
    def update_record(old, new)
      if old.nil? or new.time < old.time
        old.update_attribute :record, false unless old.nil?
        new.record = true
      end
    end
end