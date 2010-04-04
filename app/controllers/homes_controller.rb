class HomesController < ApplicationController
  def show
    @announcement = Post.order('created_at desc').includes(:user).first
  end
end