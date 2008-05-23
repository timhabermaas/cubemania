class CompetitionsController < ResourceController::Base
  permit :owner, :only => [:update, :destroy]

  belongs_to :puzzle

  show.before do
    @date = params[:date].nil? ? Time.now : Time.parse(params[:date])
  end

  [update, create].each do |action|
    action.wants.html { redirect_to puzzle_competition_path params[:puzzle_id], @competition }
  end

  private
    def collection
      @collection ||= end_of_association_chain.paginate :include => :user, :order => 'created_at desc', :page => params[:page], :per_page => 10
    end
    
    def object
      @object ||= end_of_association_chain.find params[:id], :include => :user
    end
    
    def build_object
      @object ||= current_user.competitions.build object_params
      @object.puzzle_id = params[:puzzle_id]
    end
end