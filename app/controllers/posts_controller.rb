class PostsController < ResourceController::Base
  permit :moderator, :except => [:index, :show]

  private
    def collection
      @collection ||= Post.paginate :include => :user, :order => 'created_at desc', :page => params[:page], :per_page => 5
    end
    
    def object
      @object ||= Post.find params[:id], :include => :user
    end
    
    def build_object
      @object ||= current_user.posts.build object_params
    end
end