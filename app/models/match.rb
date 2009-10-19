class Match < ActiveRecord::Base
  belongs_to :puzzle
  belongs_to :user; attr_protected :user, :user_id
  belongs_to :opponent, :class_name => 'User'
  has_many :averages
  
  named_scope :finished, :conditions => "EXISTS(SELECT clocks.* FROM clocks WHERE clocks.match_id = matches.id AND clocks.type = 'Average' AND matches.user_id = clocks.user_id) AND
            EXISTS(SELECT clocks.* FROM clocks WHERE clocks.type = 'Average' AND clocks.match_id = matches.id AND matches.opponent_id = clocks.user_id)"
  
  validates_presence_of :user_id, :opponent_id, :puzzle_id
  validate :self_challenges
  
  def finished?
    user.averages.match(id) != nil and opponent.averages.match(id) != nil
  end
  
  def pending?
    user.averages.match(id).nil? and opponent.averages.match(id).nil?
  end
  
  def challenged?
    user.averages.match(id) != nil and opponent.averages.match(id).nil?
  end
  
  private
    def self_challenges
      errors.add_to_base "You can't challenge yourself!" if user_id == opponent_id
    end
end