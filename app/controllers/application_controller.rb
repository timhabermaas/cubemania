class ApplicationController < ActionController::Base
  include Authentication

  after_filter :flash_to_headers

  login :except => [:index, :show]

  load_and_authorize_resource

  protect_from_forgery

  rescue_from ActiveRecord::RecordNotFound do |exception|
    render 'errors/not_found'
  end

  rescue_from CanCan::AccessDenied do |exception|
    flash[:notice] = 'You do not have the necessary permissions'
    redirect_to root_path
  end

  before_filter :store_return_to

private
  def store_return_to
    store_location params[:return_to] unless params[:return_to].nil?
  end

  def flash_to_headers
    return unless request.xhr?
    [:notice, :alert].each do |type|
      if flash[type].present?
        response.headers['X-Message'] = flash[type]
        response.headers['X-Type'] = type.to_s
      end
    end
    flash.discard
  end
end