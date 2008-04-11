class ApplicationController < ActionController::Base
  include Authentication
  include ExceptionNotifiable

  protect_from_forgery

  filter_parameter_logging 'password'

  before_filter :init_navigation

  def init_navigation
    @navigation = Item.find :all, :order => 'name'
    @kinds = Kind.find :all
  end
end