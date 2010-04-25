class FacebooksController < ApplicationController
  login :except => []

  def create
    redirect_to facebook_client.web_server.authorize_url(:redirect_uri => callback_facebook_url, :scope => "stream_publish")
  end

  def callback
    access_token = facebook_client.web_server.get_access_token(params[:code], :redirect_uri => callback_facebook_url)
    current_user.update_attribute :fb_access_token, access_token.token
    flash[:notice] = "You are now connected to Facebook."
    redirect_to root_path
  end
end
