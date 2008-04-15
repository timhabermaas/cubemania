module Authentication
  def self.included(base)
    base.extend ClassMethods
    base.helper_method :user, :logged_in?, :role?, :admin?, :moderator?
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
    
    def permit(role, filters = {})
      before_filter filters do |controller|
        controller.permit role
      end
    end
  end

  def permit(role)
    if role? role
      yield if block_given?
    else
      flash[:notice] = 'You do not have the necessary permissions'
      redirect_to root_url
    end
  end

  protected
    def login
      unless logged_in?
        flash[:notice] = 'Please login or <a href="/register">register</a> to continue'
        session[:return_to] = request.request_uri
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
      not user.nil?
    end
    
    def role?(role)
      user.role? role, params[:id] if logged_in?
    end
    
    def admin?
      role? :admin
    end
    
    def moderator?
      role? :moderator
    end
    
    def user
      params[:user_id] ||= session[:user_id]
      @user ||= User.find session[:user_id] unless session[:user_id].nil?
    end
    
    def user=(user)
      @user = user
      session[:user_id] = user.nil? ? nil : user.id
    end
    
    def redirect_back(default)
      redirect_to(session[:return_to] || default)
      session[:return_to] = nil
    end
end