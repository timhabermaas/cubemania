class ClocksController < ResourceController::Base
  login

  belongs_to :user

  index.before do
    params[:puzzle] ||= '4'
    params[:kind] ||= '2'
    @puzzle = Puzzle.find_by_id params[:puzzle]
  end
  
  index.wants.xml
  
  def auto_complete_for_user_name
    @users = User.find :all, :conditions => ["LOWER(name) LIKE ?", '%' + params[:val] + '%'], :order => "name ASC", :limit => 10
  end
  
  private
    def collection
      @collection ||= end_of_association_chain.find_all_by_puzzle_id params[:puzzle],
          :conditions => ['created_at >= ?', Time.now - 30*24*60*60], :order => 'created_at'
    end
end