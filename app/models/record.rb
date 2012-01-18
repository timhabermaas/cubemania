class Record < ActiveRecord::Base
  belongs_to :puzzle
  belongs_to :user

  validates_presence_of :user_id, :puzzle_id, :time, :amount, :singles
  validates_uniqueness_of :user_id, :scope => [:puzzle_id, :amount], :message => "can't have more than one record per puzzle and amount"
  validates_inclusion_of :amount, :in => [1, 5, 12]
  validate :has_as_many_singles_as_amount

  serialize :singles
  after_initialize :set_singles_to_empty_array

  humanize :time => :time

  def self.update_with!(user, puzzle, amount, time, singles) # TODO use hash
    old_record = where(:user_id => user.id,
                       :puzzle_id => puzzle.id,
                       :amount => amount).first
    if old_record && old_record.time > time
      old_record.update_attributes(:time => time, :singles => singles)
    elsif old_record.nil?
      Record.create! :user_id => user.id,
                     :puzzle_id => puzzle.id,
                     :amount => amount,
                     :time => time,
                     :singles => singles
    end
  end

  private
  def set_singles_to_empty_array
    self.singles = [] if singles.nil?
  end

  def has_as_many_singles_as_amount
    errors.add(:singles, "must have #{amount} items, but has #{singles.size}") if singles && singles.size != amount
  end
end
