module Api
  class SinglesController < ApiController
    respond_to :json
    before_filter :fetch_puzzle

    def index
      user = User.find params[:user_id]
      @singles = user.singles.for(@puzzle).limit(params[:limit] || 150)
      respond_to do |format|
        format.json
      end
    end

    def grouped
      user = User.find params[:user_id]
      @singles = user.singles.where(:puzzle_id => @puzzle.id).grouped(by: params[:by]).order("created_at desc")

      respond_to do |format|
        format.json
      end
    end

    def create
      single = current_user.singles.build(params[:single].merge(:puzzle_id => @puzzle.id))
      if single.save
        if UpdateRecentRecords.for(current_user, puzzle)
          response.headers["X-NewRecord"] = "true"
        end
      end
      respond_to do |format|
        format.json { render :json => single } # TODO why does respond_with not work?
      end
    end

    # TODO add case for failed destruction (e.g. when single is part of competition)
    def destroy
      single = current_user.singles.find params[:id]
      if single.destroy
        enqueue_record_job current_user, @puzzle
      end
      respond_with single
    end

    def update
      single = current_user.singles.find params[:id]
      single.attributes = params[:single]
      enqueue_record_job(current_user, @puzzle) if single.penalty_changed?
      if single.save
      else
        # TODO handle error case
      end
      respond_with single
    end

  private
    def enqueue_record_job(user, puzzle)
      job = RecordCalculationJob.new(user.id, puzzle.id)
      Delayed::Job.enqueue job unless job.exists?
    end

    def fetch_puzzle
      @puzzle = Puzzle.find params[:puzzle_id]
    end
  end
end
