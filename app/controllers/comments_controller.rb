class CommentsController < ApplicationController
  permit :moderator, :only => :destroy

  def create
    @comment = current_user.comments.build(params[:comment].merge :post_id => parent.id)
    @comment.save
    respond_to do |format|
      format.js
    end
  end

  def destroy
    object.destroy
    redirect_to post_path(@post, :anchor => 'comments')
  end

  private
    def object
      @comment ||= parent.comments.find params[:id]
    end
    
    def parent
      @post ||= Post.find params[:post_id]
    end
end