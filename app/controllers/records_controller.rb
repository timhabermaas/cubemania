class RecordsController < ApplicationController
  def index
    @puzzle = Puzzle.find params[:puzzle_id]
    @records = Clock.where(:record => true).
                     where(:type => params[:type].capitalize).
                     where(:puzzle_id => @puzzle.id).
                     joins(:user).where('users.ignored' => false).order(:time)
    #@records = @puzzle.records.send params[:type], params[:page]
  end
end