class FacebooksController < ApplicationController
  login :except => []

  def new
    @facebook_url = facebook_consumer.authorize_url
  end

  def callback
    access_token = facebook_consumer.authorize!(params[:code])
    current_user.update_attribute :fb_access_token, access_token
    flash[:notice] = "You are now connected to Facebook."
    redirect_back root_path
  end
end
