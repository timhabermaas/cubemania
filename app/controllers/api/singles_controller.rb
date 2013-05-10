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

        records = calculate_recent_records(single)
        records.each { |r| r.save! }
        records.each { |r| CreateActivity.for_record(r) }

        if records.present?
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
        CubingSessionManager.remove(single)
        #enqueue_record_job current_user, @puzzle
        recreate_records(single)
      end
      respond_with single
    end

    def update
      single = current_user.singles.find params[:id]
      single.attributes = params[:single]
      #enqueue_record_job(current_user, @puzzle) if single.penalty_changed?
      if single.save
        recreate_records(single) if single.penalty_changed? or single.time_changed?
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

    # TODO don't reverse every time
    def calculate_recent_records(single)
      puzzle = single.puzzle
      recent_singles = single.user.singles.for(puzzle).limit(RecordType.max_count)

      RecordType.all.map do |type|
        current_record = single.user.records.where(:puzzle_id => puzzle.id).amount(type.count).recent.first
        RecalculateRecordsHistory.for(type, recent_singles[0...type.count].reverse, current_record)
      end.flatten
    end

    # TODO if older than xy then add to job queue
    def recreate_records(single)
      InvalidateRecords.for(single)

      RecordType.all.each do |type|
        singles = Single.younger_than(single).order("created_at")
        singles_2 = Single.last_n_prior_to(type.count, single).reverse

        all_singles = singles_2 + singles
        old_record = single.user.records.where(:puzzle_id => single.puzzle_id).amount(type.count).recent.first
        RecalculateRecordsHistory.for!(type, all_singles, old_record)
      end
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
