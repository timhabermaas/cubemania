class TwitterAuthsController < ApplicationController
  login :except => []

  def new
  end

  def create
    request_token = twitter_client.request_token(:oauth_callback => confirmed_twitter_auth_url)
    session[:request_token] = request_token.token
    session[:request_secret] = request_token.secret
    redirect_to request_token.authorize_url
  end

  def destroy
    current_user.twitter_token = current_user.twitter_secret = nil
    current_user.save
    redirect_to current_user
  end

  def confirmed
    access_token = twitter_client.authorize(session[:request_token], session[:request_secret], :oauth_verifier => params[:oauth_verifier])
    current_user.twitter_token = access_token.token
    current_user.twitter_secret = access_token.secret
    if current_user.save
      flash[:notice] = "Successfully connected to Twitter"
    else
      flash[:notice] = "There was an error connecting your Twitter account to Cubemania"
    end
    redirect_back(current_user)
  end
end
