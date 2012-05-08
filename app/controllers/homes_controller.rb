class HomesController < ApplicationController
  skip_load_and_authorize_resource

  def show
    @announcement = Post.order('created_at desc').first
  end
end
