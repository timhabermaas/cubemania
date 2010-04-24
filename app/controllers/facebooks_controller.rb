class FacebooksController < ApplicationController
  login :except => []

  def create
    redirect_to facebook_consumer.authorize_url
  end

  def callback
    access_token = facebook_consumer.authorize!(params[:code])
    current_user.update_attribute :fb_access_token, access_token
    flash[:notice] = "You are now connected to Facebook."
    redirect_to root_path
  end
end
