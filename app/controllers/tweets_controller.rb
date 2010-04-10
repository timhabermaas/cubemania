class TweetsController < ApplicationController
  login :except => []
  
  before_filter :twitter

  def create
    client.update("Playing around :)")
  end
  
private
  def twitter
    if current_user.twitter_token.blank? or current_user.twitter_secret.blank?
      flash[:notice] = "You need to be connected to twitter"
      redirect_to new_twitter_auth_path
    end
  end
end
