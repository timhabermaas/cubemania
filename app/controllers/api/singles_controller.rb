module Api
  # TODO get rid of instance variables by using cached instance methods
  class SinglesController < ApiController
    respond_to :json

    before_filter :fetch_user, :only => [:index, :chart]
    before_filter :fetch_puzzle

    def index
      @singles = @user.singles.for(@puzzle).limit(params[:limit] || 150)
      respond_with @singles
    end

    def chart
      @singles = @user.singles.where(:puzzle_id => @puzzle.id).not_dnf

      begin
        from = DateTime.strptime(params[:from], "%s")
        to = DateTime.strptime(params[:to], "%s")
      rescue
        from = @singles.minimum(:created_at)
        to = Time.zone.now
      end
      difference = to - from # in days
      @singles =
        if difference > 365 * 2 # TODO 2.years
          @singles.grouped(by: :month, time_zone: @user.time_zone)
        elsif difference > 200
          @singles.grouped(by: :week, time_zone: @user.time_zone)
        elsif difference > 30
          @singles.grouped(by: :day, time_zone: @user.time_zone)
        else
          @singles.scoped
        end.where(:created_at => from..to).order("created_at")
      respond_with @singles
    end

    def create
      single = current_user.singles.build(params[:single].merge(:puzzle_id => @puzzle.id))
      if single.save
        session = CubingSessionManager.create_or_add(single)
        CreateActivity.for_cubing_session(session) if session.new?

        records = RecordCalculationJob.new(single).perform
        records.each { |r| CreateActivity.for_record(r) }

        if records.present?
          response.headers["X-New-Records"] = "true"
        end
      end

      respond_with :api, @puzzle, single
    end

    # TODO add case for failed destruction (e.g. when single is part of competition)
    def destroy
      single = current_user.singles.find params[:id]
      if single.destroy
        CubingSessionManager.remove(single)
        enqueue_record_job(single)
      end
      response.headers["X-Refetch-Records"] = "true"
      respond_with single
    end

    def update
      single = current_user.singles.find params[:id]
      single.attributes = params[:single]
      if single.save
        enqueue_record_job(single) # TODO only if penalty or time was updated
      else
        # TODO handle error case; add test
      end
      response.headers["X-Refetch-Records"] = "true"
      respond_with single
    end

  private
    def enqueue_record_job(single)
      job = RecordCalculationJob.new(single)
      Delayed::Job.enqueue job unless job.exists? # TODO only if single is really old
    end

    def fetch_user
      @user = User.find params[:user_id]
    end

    def fetch_puzzle
      @puzzle = Puzzle.find params[:puzzle_id]
    end
=begin
    def grouped_cache_path(user, puzzle, by)
      ["grouped", user.id, puzzle.id, by].join("/")
    end
=end
  end
end
