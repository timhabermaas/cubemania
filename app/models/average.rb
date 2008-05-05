class Average < Clock
  belongs_to :user, :counter_cache => true; attr_protected :user_id, :user
  belongs_to :puzzle
  has_many :singles, :order => 'position', :dependent => :destroy
  
  before_save :update_records_on_create
  after_destroy :update_records_on_destroy

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
      update_record user.averages.record(puzzle_id), self
      update_record user.singles.record(puzzle_id), singles.reject(&:dnf).sort_by(&:time).first
    end

    def update_record(old, new)
      if not new.nil? and not new.dnf? and (old.nil? or new.time < old.time)
        old.update_attribute :record, false unless old.nil?
        new.record = true
      end
    end
    
    def update_records_on_destroy
      unless singles.select(&:record).empty?
        best_single = user.singles.for(puzzle).first
        best_single.update_attribute :record, true unless best_single.nil?
      end
      if record
        best_average = user.averages.for(puzzle).sort{|a,b| a.time <=> b.time}.first
        best_average.update_attribute :record, true unless best_average.nil?
      end
    end
end