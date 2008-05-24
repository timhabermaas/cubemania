class Clock < ActiveRecord::Base
  belongs_to :puzzle
  belongs_to :competition
  belongs_to :user; attr_protected :user_id, :user

  validates_presence_of :time, :puzzle_id, :user_id
  validates_length_of :comment, :maximum => 255, :allow_nil => true
  validates_length_of :scramble, :maximum => 1024, :allow_nil => true
  
  def validate
    unless competition_id.nil?
      unless Clock.find_by_user_id_and_puzzle_id_and_competition_id(user_id, puzzle_id, competition_id, :conditions => ['created_at between ? and ?', competition.started_at, competition.ended_at]).nil?
        errors.add_to_base 'Get out of here!'
      end
    end
  end
end