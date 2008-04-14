class UsersController < ResourceController::Base
  skip_login :only => [:new, :create]
  logout :only => [:new, :create]
  permit :self, :only => [:edit, :update, :destroy]

  create do
    flash "Hello, you are now registered"
    after { self.user = @user }
  end

  destroy.after { self.user = nil }
end