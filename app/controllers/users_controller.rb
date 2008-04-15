class UsersController < ResourceController::Base
  skip_login :only => [:new, :create]
  logout :only => [:new, :create]
  permit :self, :only => [:edit, :update, :destroy]

  index.before { @max_clocks_count = User.max_clocks_count }

  create do
    flash 'Hello, you are now registered'
    after { self.user = @user }
  end

  destroy.after { self.user = nil }
  
  private
    def collection
      @collection ||= User.find :all, :order => 'name'
    end
end