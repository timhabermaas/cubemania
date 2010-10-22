class Single < ActiveRecord::Base
  belongs_to :user, :counter_cache => true
  belongs_to :puzzle

  attr_accessible :time, :human_time, :puzzle_id, :scramble

  validates_presence_of :user_id, :puzzle_id, :time
  validates_format_of :human_time, :with => /\A(\d+:)?(\d+:)?\d+(.\d+)?\s*(min|s|h)?\Z/

  before_validation :set_time, :if => :human_time_is_set
  after_save :update_average_record

  def human_time(spacer = '')
    t = self.time || 0
    hs = (t / 10.0).round
    if hs >= 6000
      min = hs / 6000
      sec = (hs - min * 6000) / 100.0
      '%d:%05.2f' % [min, sec] + spacer + 'min' # 12.555 => "12.55"
    else
      '%.2f' % (hs.to_f / 100) + spacer + 's'
    end
  end

  # TODO check for correct format. option 1: validates_format_of + before_save, option 2: don't know
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
  def update_average_record
    last_singles = Single.where(:user_id => user_id).where(:puzzle_id => puzzle_id).order("created_at desc").limit(5)

    avg = Average.new(last_singles).average

    logger.info("New average: #{avg}")

    old_average = AverageRecord.where(:puzzle_id => puzzle_id, :user_id => user_id).first
    logger.info("old average: #{old_average.try(:time)}")
    if old_average.nil?
      new_average = AverageRecord.new(:puzzle_id => puzzle_id, :user_id => user_id, :time => avg)
      new_average.singles = last_singles
      new_average.save!
    elsif avg < old_average.time
      old_average.update_attribute :time, avg
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
