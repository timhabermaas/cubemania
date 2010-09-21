class AverageRecord < ActiveRecord::Base
  belongs_to :user
  belongs_to :puzzle

  validates_presence_of :user_id, :puzzle_id, :time, :single_ids
  validates_uniqueness_of :user_id, :scope => :puzzle_id

  def singles=(s)
    self.single_ids = s.map(&:id).join(";")
  end

  def singles
    Single.find(single_ids.split(";"))
  end

end
