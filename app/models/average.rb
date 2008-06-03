class Average < Clock
  belongs_to :user, :counter_cache => true; attr_protected :user_id, :user
  has_many :singles, :order => 'position', :dependent => :destroy
  
  before_save :update_records_on_create
  after_destroy :update_records_on_destroy

  def validate
    unless competition_id.nil?
      unless Clock.find_by_user_id_and_puzzle_id_and_competition_id_and_type(user_id, puzzle_id, competition_id, 'Average', :conditions => ['created_at between ? and ?', competition.started_at, competition.ended_at]).nil?
        errors.add_to_base 'Get out of here!'
      end
    end
  end

  def notice
    if singles.select(&:record).empty?
      return 'You have a new personal average record!' if record?
    else
      return 'You have a new personal single and average record!' if record?
      return 'You have a new personal single record!'
    end
  end

  private
    def update_records_on_create
      update_record_on_create user.averages.record(puzzle_id), self
      update_record_on_create user.singles.record(puzzle_id), singles.reject(&:dnf).sort_by(&:time).first
    end

    def update_record_on_create(old, new)
      if not new.nil? and not new.dnf? and (old.nil? or new.time < old.time)
        old.update_attribute :record, false unless old.nil?
        new.record = true
      end
    end
    
    def update_records_on_destroy
      update_record_on_destroy user.averages.best(puzzle.id) if record?
      update_record_on_destroy user.singles.best(puzzle.id) unless singles.select(&:record).empty?
    end
    
    def update_record_on_destroy(new)
      new.update_attribute :record, true unless new.nil?
    end
end