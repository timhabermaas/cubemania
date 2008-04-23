class Average < Clock
  belongs_to :user, :counter_cache => true; attr_protected :user_id, :user
  has_many :singles, :order => 'time', :dependent => :destroy
  
  before_save :update_records
  
private
  def update_records
    current_user = User.find self.user_id
    average_record = current_user.averages.record(self.puzzle_id)
    single_record = current_user.singles.record(self.puzzle_id)
    update_record average_record, self
    update_record single_record, self.singles.first
  end

  def update_record(old, new)
    if (old.nil? or new.time < old.time) and not new.dnf?
      old.update_attribute :record, false unless old.nil?
      new.record = true
    end
  end
end