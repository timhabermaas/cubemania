class CompetitionsController < ResourceController::Base
  permit :owner, :only => [:update, :destroy]

  belongs_to :puzzle

  show.before do
    @date = params[:date].nil? ? Time.now : Time.parse(params[:date])
    scramble = @competition.scrambles.first # no need to load this, if the user visits an old competition, but i don't want to make it too complicated for now
    if scramble.nil? or @competition.old? scramble.created_at
      scramble = @competition.scrambles.build :scrambles => @competition.puzzle.all_scrambles
      scramble.save
    end
    @scrambles = scramble.scrambles
  end

  [create, update].each do |action|
    action.wants.js; action.failure.wants.js
  end

  private
    def collection
      @collection ||= end_of_association_chain.paginate :include => :user, :order => 'sticky desc, created_at desc', :page => params[:page], :per_page => 10
    end
    
    def object
      @object ||= end_of_association_chain.find params[:id], :include => :user
    end
    
    def build_object
      @object ||= current_user.competitions.build object_params
      @object.puzzle_id = params[:puzzle_id]
    end
end