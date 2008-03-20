# Filters added to this controller apply to all controllers in the application.
# Likewise, all the methods added will be available for all controllers.

class ApplicationController < ActionController::Base
  # Pick a unique cookie name to distinguish our session data from others'
  session :session_key => '_Cubemania_session_id'

  before_filter :init_navigation
  
  def init_navigation
    @navigation = Item.find :all, :order => :name
    @kinds = Kind.find :all, :include => :puzzles
  end
end
