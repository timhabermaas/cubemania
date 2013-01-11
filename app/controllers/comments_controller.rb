class CommentsController < ApplicationController
  def create
    @comment = current_user.comments.build(params[:comment].merge :post_id => parent.id)

    if @comment.save
      respond_to do |format|
        format.js
        format.html { redirect_to @comment.post }
      end
    else
      respond_to do |format|
        format.js { render 'create.failure' }
        format.html { redirect_to @comment.post }
      end
    end
  end

  def destroy
    object.destroy
    respond_to do |format|
      format.js
      format.html { redirect_to object.post }
    end
  end

  private
    def object
      @comment ||= parent.comments.find params[:id]
    end

    def parent
      @post ||= Post.find params[:post_id]
    end
end