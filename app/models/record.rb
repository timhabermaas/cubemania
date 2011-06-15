class Record < ActiveRecord::Base
  belongs_to :user
  belongs_to :puzzle

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

  def update_with_single!(single)
    self.time = single.time
    self.singles = [single]
    save
  end

  class << self
    def calculate_for!(user_id, puzzle_id, format)
      ra = RollingAverage.new(format)
      current_record = Record.where(:user_id => user_id, :puzzle_id => puzzle_id, :amount => format).first
      best = nil
      best_singles = nil
      Single.where(:user_id => user_id).where(:puzzle_id => puzzle_id).find_each do |single|
        ra << single
        if best.nil? or (ra.average and ra.average < best)
          best = ra.average
          best_singles = ra.singles
        end
      end
      if best
        if current_record
          current_record.update_attributes(:time => best, :singles => best_singles)
        else
          Record.create!(:user_id => user_id, :puzzle_id => puzzle_id, :amount => format, :time => time, :singles => best_singles)
        end
      else
        if current_record
          current_record.destroy
        end
      end
    end
    handle_asynchronously :calculate_for!
  end
end
