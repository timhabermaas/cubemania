class ClocksController < ResourceController::Base
  login

  belongs_to :user

  index.before do
    params[:puzzle] ||= '4'
    params[:kind] ||= '2'
  end
  
  private
    def collection
      @collection ||= end_of_association_chain.find_all_by_puzzle_id params[:puzzle], :conditions => ['created_at >= ?', Time.now - 30*24*60*60]
    end
end