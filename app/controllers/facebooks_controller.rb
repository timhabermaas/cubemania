class FacebooksController < ApplicationController
  def new
    @facebook_url = facebook_consumer.authorize_url
  end

  def callback
    access_token = facebook_consumer.authorize!(params[:code])
    current_user.update_attribute :fb_access_token, access_token
    redirect_back root_path
  end
end
