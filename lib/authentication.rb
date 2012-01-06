module Authentication
  def self.included(base)
    base.extend ClassMethods
    base.helper_method :current_user, :current_user?, :logged_in?, :role?, :self?, :owner?, :admin?, :moderator?
  end

  module ClassMethods
    def login(filters = {})
      before_filter :login, filters
    end

    def skip_login(filters = {})
      skip_before_filter :login, filters
    end

    def logout(filters = {})
      before_filter :logout, filters
    end

#    def permit(role, filters = {})
#      before_filter filters do |controller|
#        controller.permit role
#      end
#    end
  end

#  def permit(role)
#    if role? role
#      yield if block_given?
#    else
#      flash[:notice] = 'You do not have the necessary permissions'
#      redirect_to root_url
#    end
#  end

  protected
    def login
      unless logged_in?
        flash[:notice] = 'Please login or <a href="/register">register</a> to continue'.html_safe
        store_location
        redirect_to login_path
      end
    end

    def logout
      if logged_in?
        flash[:notice] = 'You must logout before you can login or register'
        redirect_to root_url
      end
    end

    def logged_in?
      not current_user.nil?
    end

    def current_user?(user)
      user == current_user || admin?
    end

    def role?(role)
      current_user.role? role, params[:id], object_if_available if logged_in?
    end

    def self?
      role? :self
    end

    def owner?
      role? :owner
    end

    def admin?
      role? :admin
    end

    def moderator?
      role? :moderator
    end

    def current_user
      params[:user_id] ||= session[:user_id].to_s
      @current_user ||= User.find session[:user_id] unless session[:user_id].nil?
    rescue ActiveRecord::RecordNotFound
      session[:user_id] = nil
    end

    def current_user=(user)
      @current_user = user
      session[:user_id] = user.nil? ? nil : user.id
    end

    def redirect_back(default)
      redirect_to(session[:return_to] || default)
      session[:return_to] = nil
    end

    def store_location(return_to = request.fullpath)
      session[:return_to] = return_to
    end

  private
    def object_if_available
      object if [:show, :edit, :update, :destroy].include? params[:action].to_sym
    rescue ActiveRecord::RecordNotFound
      nil
    end
end