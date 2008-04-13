class PostsController < ResourceController::Base
  login :role => :moderator, :except => [ :index, :show ]

  private
    def collection
      @collection ||= end_of_association_chain.paginate :page => params[:page], :per_page => 1, :order => 'updated_at desc'
    end
    
    def build_object
      @object ||= user.posts.build object_params
    end
end