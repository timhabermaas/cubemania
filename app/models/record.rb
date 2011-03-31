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
end
