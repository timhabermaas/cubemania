module Api
  class UsersController < ApiController
    def index
      @users = User.order('singles_count desc').paginate(:page => params[:page], :per_page => 200)
      @users = @users.where('lower(name) LIKE ?', "%#{params[:q].downcase}%") if params[:q]

      respond_to do |format|
        format.html
        format.json
      end
    end

    def show
      @user = object

      respond_to do |format|
        format.html
        format.json
      end
    end

    def block
      @user = object
      @user.block!

      respond_to do |format|
        format.json { render :json => @user }
      end
    end

  private
    def object(options = nil)
      User.find params[:id], options
    end
  end
end
