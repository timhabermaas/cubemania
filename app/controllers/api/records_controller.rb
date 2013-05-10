module Api
  class RecordsController < ApiController
    respond_to :json

    def index
      @records = user.records.where(:puzzle_id => puzzle.id)
      respond_with :api, puzzle, @records
    end

    def recent
      @records = RecordType.all.map do |type|
        user.records.where(:puzzle_id => puzzle.id, :amount => type.count).
                     recent.first
      end.compact
      respond_with :api, puzzle, @records
    end

    private
    def puzzle
      @puzzle ||= Puzzle.find params[:puzzle_id]
    end

    def user
      @user ||= User.find params[:user_id]
    end
  end
end
