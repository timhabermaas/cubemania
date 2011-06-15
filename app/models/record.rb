class Record < ActiveRecord::Base
  belongs_to :puzzle
  belongs_to :user

  validates_presence_of :user_id, :puzzle_id, :time, :amount, :single_ids
  validates_uniqueness_of :user_id, :scope => [:puzzle_id, :amount]
  validates_inclusion_of :amount, :in => [1, 5, 12]

  humanize :time => :time

  def singles=(s)
    self.single_ids = s.map(&:id).join(';')
  end

  def singles
    Single.find(single_ids.split(';'))
  end

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
end
