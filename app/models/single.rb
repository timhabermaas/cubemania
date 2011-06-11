class Single < ActiveRecord::Base
  belongs_to :user, :counter_cache => true
  belongs_to :puzzle

  attr_accessible :time, :human_time, :puzzle_id, :scramble, :penalty

  validates_presence_of :user_id, :puzzle_id, :time

  before_validation :set_time, :if => :human_time_is_set
  after_create :update_average_records_after_create, :update_single_record
  after_destroy :update_single_record
  after_update :update_single_record

  scope :not_dnf, where("penalty IS NULL OR penalty NOT LIKE 'dnf'")

  humanize :time => :time

  # TODO check for correct format. option 1: validates_format_of + before_save, option 2: don't know, option 3: don't care at all
  def human_time=(ht)
    @human_time = ht
  end

  def toggle_dnf!
    if self.dnf?
      self.update_attribute :penalty, nil
    else
      self.update_attribute :penalty, "dnf"
    end
  end

  def dnf?
    self.penalty == "dnf"
  end

private
  def update_average_records_after_create
    [5, 12].each do |amount|
      last_singles = Single.where(:user_id => user_id).where(:puzzle_id => puzzle_id).order("created_at desc").limit(amount)

      next if last_singles.size < amount

      avg = RollingAverage.new(amount, last_singles).average

      return if avg.nil?

      old_record = Record.where(:puzzle_id => puzzle_id, :user_id => user_id, :amount => amount).first

      if old_record.nil?
        new_average = Record.new(:puzzle_id => puzzle_id, :user_id => user_id, :time => avg, :singles => last_singles, :amount => amount)
        new_average.singles = last_singles
        new_average.save!
      elsif avg < old_record.time
        old_record.time = avg
        old_record.singles = last_singles
        old_record.save!
      end
    end
  end

  def update_single_record
    single_record = Record.where(:puzzle_id => puzzle_id, :user_id => user_id, :amount => 1).first
    # actually we don't need to set a new record if the destroyed single is a dnf, but since we use this method for destroying and updating, we can't ignore it
    if single_record.nil?
      fastest = user.singles.best(puzzle_id)
      unless fastest.nil?
        user.records.create(:puzzle_id => puzzle_id, :time => fastest.time, :amount => 1, :singles => [fastest])
      end
    else
      if self.time <= single_record.time
        fastest = user.singles.best(puzzle_id)
        if fastest.nil?
          single_record.destroy
        else
          # single_record.update_with_singles!(fastest)
          single_record.update_with_single!(fastest)
        end
      end
    end

  end

  def set_time
    seconds, minutes, hours = @human_time.split(':').reverse
    self.time = (hours.to_i * 3600 + minutes.to_i * 60) * 1000 + (seconds.to_f * 1000).to_i
  end

  def human_time_is_set
    not @human_time.blank?
  end
end
