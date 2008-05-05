class UsersController < ResourceController::Base
  skip_login :only => [:new, :create, :password]
  logout :only => [:new, :create]
  permit :self, :only => [:edit, :update, :destroy]
  permit :moderator, :only => []

  index.before { @max_averages_count = User.max_averages_count }

  show.before { @records = @user.averages.records }

  create do
    flash 'Hello, you are now registered'
    after { self.current_user = @user }
    wants.html { redirect_back user_path(@user) }
  end

  destroy.after { if self.current_user == @user; self.current_user = nil; end }
  
  private
    def collection
      @collection ||= User.find :all, :order => 'name'
    end
end