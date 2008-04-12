class Clock < ActiveRecord::Base
  belongs_to :puzzle
  belongs_to :user; attr_protected :user_id, :user

  validates_presence_of :time, :puzzle_id
  
  def self.single
    find_all_by_type 'Single'
  end
  
  def self.average
    find_all_by_type 'Average'
  end
end