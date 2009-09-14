class UsersController < ResourceController::Base
  skip_login :only => [:new, :create]
  logout :only => [:new, :create]
  permit :self, :only => [:edit, :update, :destroy]
  #protect [:role, :sponsor, :ignored], :but => :admin, :only => [:create, :update]

  index.before { @max_averages_count = User.max_averages_count }
  
  index do
    wants.json { render :json => @users.to_json }
  end

  show.before do
    single_records, average_records = @user.singles.records, @user.averages.records
    @records = (0...single_records.size).map do |i|
      unless average_records[i].nil? or single_records[i].puzzle_id == average_records[i].puzzle_id
        average_records.insert i, nil
      end
      { :single => single_records[i], :average => average_records[i] }
    end
    @participations = @user.participations
  end

  create do
    flash { "Hello #{@user.name}, you are now registered" }
    after { self.current_user = @user }
    wants.html { redirect_back user_path(@user) }
  end

  destroy.after { if self.current_user == @user; self.current_user = nil; end }
  
  private
    def collection
      @collection ||=
        if params[:search]
          User.paginate :page => params[:page], :per_page => 100, :order => 'name', :conditions => ['name LIKE ?', "%#{params[:search]}%"]
        else
          User.paginate :page => params[:page], :per_page => 100, :order => 'name'
        end
    end
end