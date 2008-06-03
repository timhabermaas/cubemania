class CompetitionsController < ResourceController::Base
  permit :owner, :only => [:update, :destroy]

  belongs_to :puzzle

  show.before do
    @date = params[:date].nil? ? Time.now : Time.parse(params[:date])
    unless @competition.old? @date
      scrambles = @competition.scrambles.for @competition, @date
      if scrambles.empty?
        @scrambles = @competition.create_scrambles
      else
        @scrambles = scrambles.map(&:scramble)
      end
    end
  end

  [create, update].each do |action|
    action.wants.js; action.failure.wants.js
  end

  private
    def collection
      params[:repeat] ||= 'all'
      options = { :include => :user, :order => 'sticky desc, created_at desc', :page => params[:page], :per_page => 10 }
      if params[:repeat] == 'all'
        @collection ||= end_of_association_chain.paginate options
      else
        @collection ||= end_of_association_chain.paginate_by_repeat params[:repeat], options
      end
    end
    
    def object
      @object ||= end_of_association_chain.find params[:id], :include => :user
    end
    
    def build_object
      @object ||= current_user.competitions.build object_params
      @object.puzzle_id = params[:puzzle_id]
    end
end