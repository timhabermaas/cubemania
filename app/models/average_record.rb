class AverageRecord < ActiveRecord::Base
  belongs_to :user
  belongs_to :puzzle
  
  validates_presence_of :user_id, :puzzle_id, :time, :singles_string
  validates_uniqueness_of :user_id, :scope => :puzzle_id
  
  def singles=(s)
    self.singles_string = s.map(&:id).join(";")
  end
  
  def singles
    Single.find(singles_string.split(";"))
  end
  
end
