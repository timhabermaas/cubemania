class TwitterAuthsController < ApplicationController
  login :except => []

  def new
  end

  def create
    oauth.set_callback_url(confirmed_twitter_auth_url)
    session[:request_token] = oauth.request_token.token
    session[:request_secret] = oauth.request_token.secret
    redirect_to oauth.request_token.authorize_url
  end
  
  def confirmed
    oauth.authorize_from_request(session[:request_token],
                                 session[:request_secret],
                                 params[:oauth_verifier])

    
    current_user.twitter_token = oauth.access_token.token,
    current_user.twitter_secret = oauth.access_token.secret
    if current_user.save
      flash[:notice] = "Successfully connected to Twitter"
    else
      flash[:notice] = "There was an error connecting your Twitter account to Cubemania"
    end
    redirect_to root_path
  end
end
