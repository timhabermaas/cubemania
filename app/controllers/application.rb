class ApplicationController < ActionController::Base
  session :session_key => '_Cubemania_session_id'

  before_filter :init_navigation
  
  def init_navigation
    @navigation = Item.find :all, :order => :name
    @kinds = Kind.find :all, :include => :puzzles
  end
end