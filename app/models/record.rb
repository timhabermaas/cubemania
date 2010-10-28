class Record < ActiveRecord::Base
  belongs_to :user
  belongs_to :puzzle

  validates_presence_of :user_id, :puzzle_id, :time, :single_ids, :amount
  validates_uniqueness_of :user_id, :scope => [:puzzle_id, :amount]
  validates_inclusion_of :amount, :in => [1, 5, 12]

  humanize :time => :time

  def singles=(s)
    self.single_ids = s.map(&:id).join(";")
  end

  def singles
    Single.find(single_ids.split(";"))
  end
end
