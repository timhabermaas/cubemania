class ApplicationController < ActionController::Base
  include Authentication

  login :except => [:index, :show]

  load_and_authorize_resource

  protect_from_forgery

  rescue_from ActiveRecord::RecordNotFound do |exception|
    render "errors/not_found"
  end

  rescue_from CanCan::AccessDenied do |exception|
    flash[:notice] = "You do not have the necessary permissions!"
    redirect_to root_path
  end

  before_filter :store_return_to

private
  def store_return_to
    store_location params[:return_to] unless params[:return_to].nil?
  end
end
