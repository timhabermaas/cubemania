class UsersController < ResourceController::Base
  skip_login :only => [:new, :create]
  logout :only => [:new, :create]
  permit :self, :only => [:edit, :update, :destroy]

  index.before { @max_averages_count = User.max_averages_count }

  show.before { @records = @user.singles.records.map{|s| {:single => s, :average => @user.averages.record(s.puzzle)} } }

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