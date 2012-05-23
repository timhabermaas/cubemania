module Api
  class RecordsController < BaseController
    def index
      @puzzle = Puzzle.find params[:puzzle_id]

      @records =
      if params[:user_id]
        user = User.find params[:user_id]
        user.records.where(:puzzle_id => @puzzle.id)
      else
        params[:type] ||= "avg5"

        if params[:type] == "single"
          @puzzle.records.amount(1)
        elsif params[:type] == "avg12"
          @puzzle.records.amount(12)
        else
          @puzzle.records.amount(5)
        end.paginate(:page => params[:page], :per_page => 50)
      end

      respond_to do |format|
        format.html
        format.json
      end
    end
  end
end
