class Average < Clock
  belongs_to :user, :counter_cache => true; attr_protected :user_id, :user
  has_many :singles, :order => 'position', :dependent => :destroy
  
  before_save :update_records

  def notice
    if singles.select(&:record).empty?
      return 'You have a new personal average record!' if record?
    else
      return 'You have a new personal single and average record!' if record?
      return 'You have a new personal single record!'
    end
  end

  private
    def update_records
      update_record user.averages.record(puzzle_id), self
      update_record user.singles.record(puzzle_id), singles.sort_by(&:time).first
    end

    def update_record(old, new)
      if (old.nil? or new.time < old.time) and not new.dnf?
        old.update_attribute :record, false unless old.nil?
        new.record = true
      end
    end
end