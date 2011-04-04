class CommentsController < ApplicationController
  def create
    @comment = current_user.comments.build(params[:comment].merge :post_id => parent.id)
    @comment.save
    respond_to do |format|
      format.js
      format.html { redirect_to @comment.post }
    end
  end

  def destroy
    object.destroy
    redirect_to post_path(parent, :anchor => 'comments')
  end

  private
    def object
      @comment ||= parent.comments.find params[:id]
    end

    def parent
      @post ||= Post.find params[:post_id]
    end
end