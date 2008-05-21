class CompetitionsController < ResourceController::Base
  belongs_to :puzzle

  [update, create].each do |action|
    action.wants.html { redirect_to puzzle_competition_path params[:puzzle_id], @competition }
  end

  private
    def collection
      @collection ||= @puzzle.competitions.paginate :include => :user, :order => 'created_at desc', :page => params[:page], :per_page => 10
    end
    
    def object
      @object ||= @puzzle.competitions.find params[:id], :include => :user
    end
    
    def build_object
      @object ||= current_user.competitions.build object_params
      @object.puzzle_id = params[:puzzle_id]
    end
end