class Clock < ActiveRecord::Base
  belongs_to :puzzle
  belongs_to :competition
  belongs_to :user; attr_protected :user_id, :user
  belongs_to :competition

  validates_presence_of :time, :puzzle_id, :user_id
  validates_length_of :comment, :maximum => 255, :allow_nil => true
  validates_length_of :scramble, :maximum => 1024, :allow_nil => true
  validates_uniqueness_of :competition_id, :scope => [:puzzle_id, :user_id], :allow_nil => true
  validate :user_must_belong_to_competition
  
  def user_must_belong_to_competition
    if not competition.nil? and not competition.users.include?(user)
      errors.add_to_base "User must be part of the competition"
    end
  end
end