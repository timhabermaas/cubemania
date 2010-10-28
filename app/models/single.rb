class Single < ActiveRecord::Base
  belongs_to :user, :counter_cache => true
  belongs_to :puzzle

  attr_accessible :time, :human_time, :puzzle_id, :scramble, :dnf

  validates_presence_of :user_id, :puzzle_id, :time
  validates_format_of :human_time, :with => /\A(\d+:)?(\d+:)?\d+(.\d+)?\s*(min|s|h)?\Z/

  before_validation :set_time, :if => :human_time_is_set
  after_create :update_records

  humanize :time => :time

  # TODO check for correct format. option 1: validates_format_of + before_save, option 2: don't know, option 3: don't care at all
  def human_time=(ht)
    @human_time = ht
  end

  def toggle_dnf!
    if self.dnf?
      self.update_attribute :dnf, false
    else
      self.update_attribute :dnf, true
    end
  end

private
  def update_records
    [1, 5, 12].each do |amount|
      last_singles = Single.where(:user_id => user_id).where(:puzzle_id => puzzle_id).order("created_at desc").limit(amount)

      next if last_singles.size < amount

      avg = RollingAverage.new(amount, last_singles).average

      return if avg.nil?

      old_average = Record.where(:puzzle_id => puzzle_id, :user_id => user_id, :amount => amount).first

      if old_average.nil?
        new_average = Record.new(:puzzle_id => puzzle_id, :user_id => user_id, :time => avg, :singles => last_singles, :amount => amount)
        new_average.singles = last_singles
        new_average.save!
      elsif avg < old_average.time
        old_average.update_attribute :time, avg
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
