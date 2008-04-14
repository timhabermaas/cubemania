class ApplicationController < ActionController::Base
  include Authentication
  include ExceptionNotifiable

  login :except => [:index, :show]

  protect_from_forgery

  filter_parameter_logging 'password'
end