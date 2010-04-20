class TweetsController < ApplicationController
  login :except => []

  before_filter :twitter_auth_required

  def create
    @average = last_average
    if Rails.env.production?
      twitter_client.update(render_to_string(:partial => 'tweet.text'))
    else
      logger.info "[Tweeted]" + render_to_string(:partial => 'tweet.text')
    end
    flash[:notice] = "Tweeted your new record. <a href='http://twitter.com/#{twitter_client.info['screen_name']}'>Check it out!</a>".html_safe
    redirect_back(root_path)
  end

private
  def twitter_auth_required
    unless twitter_client.authorized? # will raise exception, catch it
      flash[:notice] = "You are not yet connected to twitter"
      redirect_to new_twitter_auth_path
    end
  end
end
