class Clock < ActiveRecord::Base
  include Comparable
  
  belongs_to :puzzle
  belongs_to :competition
  belongs_to :match
  belongs_to :user; attr_protected :user_id, :user

  validates_presence_of :time, :puzzle_id, :user_id
  validates_length_of :comment, :maximum => 255, :allow_nil => true
  validates_length_of :scramble, :maximum => 1024, :allow_nil => true
  
  def <=>(other)
    if dnf and other.dnf?
      0
    elsif dnf
      1
    elsif other.dnf?
      -1
    else
      time <=> other.time
    end
  end
end