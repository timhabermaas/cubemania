class Record < ActiveRecord::Base
  belongs_to :puzzle
  belongs_to :user
  has_and_belongs_to_many :singles do
    def ordered
      order("singles.created_at")
    end
  end

  validates_presence_of :user_id, :puzzle_id, :time, :amount, :singles, :set_at
  validates_uniqueness_of :user_id, :scope => [:puzzle_id, :amount], :message => "can't have more than one record per puzzle and amount"
  validates_inclusion_of :amount, :in => [1, 5, 12]
  validates_length_of :comment, :maximum => 255
  validate :has_as_many_singles_as_amount

  before_validation :set_set_at, :set_comments_from_singles

  humanize :time => :time

  def self.update_with!(user, puzzle, amount, time, singles, force = false) # TODO use hash
    old_record = where(:user_id => user.id,
                       :puzzle_id => puzzle.id,
                       :amount => amount).first
    if old_record && (force || old_record.time > time)
      old_record.update_attributes(:time => time, :singles => singles)
    elsif old_record.nil?
      Record.create! :user_id => user.id,
                     :puzzle_id => puzzle.id,
                     :amount => amount,
                     :time => time,
                     :singles => singles
    end
  end

  def self.remove!(user, puzzle, amount)
    record = where(:user_id => user.id, :puzzle_id => puzzle.id, :amount => amount).first
    record.destroy if record
  end

  def cubing_average
    CubingAverage.new singles, time
  end

  private
  def has_as_many_singles_as_amount
    errors.add(:singles, "must have #{amount} items, but has #{singles.size}") if singles && singles.size != amount
  end

  def set_set_at
    unless singles.empty?
      self.set_at = singles.last.created_at || Time.now
    end
  end

  def set_comments_from_singles
    self.comment = singles.map(&:comment).uniq.reject(&:blank?).join("; ")[0..254]
  end
end
