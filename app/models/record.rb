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


  class << self
    def calculate_for!(user_id, puzzle_id, format)
      best = nil
      best_singles = nil
      current_record = Record.where(:user_id => user_id, :puzzle_id => puzzle_id, :amount => format).first
      if format == 1
        best_singles = [Single.not_dnf.where(:user_id => user_id, :puzzle_id => puzzle_id).order('time').first]
        best = best_singles[0].try(:time)
      else
        ra = RollingAverage.new(format)
        Single.where(:user_id => user_id).where(:puzzle_id => puzzle_id).find_each do |single|
          ra << single
          if best.nil? or (ra.average and ra.average < best)
            best = ra.average
            best_singles = ra.singles
          end
        end
      end
      if best
        if current_record
          current_record.update_attributes(:time => best, :singles => best_singles)
        else
          Record.create!(:user_id => user_id, :puzzle_id => puzzle_id, :amount => format, :time => best, :singles => best_singles)
        end
      else
        current_record.try(:destroy)
      end
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
