class CommentsController < ApplicationController
  permit :moderator, :only => :destroy

  def create
    @post = object
    @comment = current_user.comments.build params[:comment].merge :post_id => @post.id
    @comment.save
  end
  
  def destroy
    @post = object
    @comment = @post.comments.find params[:id]
    @comment.destroy
    redirect_to post_path @post, :anchor => 'comments'
  end
  
  private
    def object
      Post.find params[:post_id]
    end
end