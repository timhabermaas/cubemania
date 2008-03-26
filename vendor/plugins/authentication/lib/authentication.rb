module Authentication
  def self.included(base)
    base.extend ClassMethods
    base.helper_method :user, :logged_in?
  end

  module ClassMethods
    def login(filters = {})
      before_filter :login, filters
    end
    
    def logout(filters = {})
      before_filter :logout, filters
    end
  end

  protected
    def login
      unless logged_in?
        flash[:notice] = 'Please login to continue'
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
    
    def user
      params[:user_id] = session[:user_id]
      @user ||= User.find session[:user_id] unless session[:user_id].nil?
    end
    
    def user=(user)
      @user = user
      session[:user_id] = params[:user_id] = user.nil? ? nil : user.id
    end
end