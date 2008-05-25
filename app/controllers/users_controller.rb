class UsersController < ResourceController::Base
  skip_login :only => [:new, :create]
  logout :only => [:new, :create]
  permit :self, :only => [:edit, :update, :destroy]

  index.before { @max_averages_count = User.max_averages_count }

  show.before do
    single_records, average_records = @user.singles.records, @user.averages.records
    @records = (0...single_records.size).map do |i|
      unless average_records[i].nil? or single_records[i].puzzle_id == average_records[i].puzzle_id
        average_records.insert i, nil
      end
      { :single => single_records[i], :average => average_records[i] }
    end
  end

  create do
    flash { "Hello #{@user.name}, you are now registered" }
    after { self.current_user = @user }
    wants.html { redirect_back user_path(@user) }
  end

  destroy.after { if self.current_user == @user; self.current_user = nil; end }
  
  private
    def collection
      @collection ||= User.find :all, :order => 'name'
    end
end