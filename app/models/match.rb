class Match < ActiveRecord::Base
  belongs_to :puzzle
  belongs_to :user; attr_protected :user, :user_id
  belongs_to :opponent, :class_name => 'User'
  has_many :averages do
    def for(user_id)
      find_by_user_id(user_id)
    end
  end
  has_many :scrambles, :as => :matchable, :order => 'position'
  
  named_scope :finished, :include => [:user, :opponent], :conditions => "EXISTS(SELECT clocks.id FROM clocks WHERE clocks.match_id = matches.id AND clocks.type = 'Average' AND matches.user_id = clocks.user_id) AND
            EXISTS(SELECT clocks.id FROM clocks WHERE clocks.type = 'Average' AND clocks.match_id = matches.id AND matches.opponent_id = clocks.user_id)"
  named_scope :challenged, :include => [:user, :opponent], :conditions => "EXISTS(SELECT clocks.id FROM clocks WHERE clocks.match_id = matches.id AND clocks.type = 'Average' AND matches.user_id = clocks.user_id) AND
            NOT EXISTS(SELECT clocks.id FROM clocks WHERE clocks.type = 'Average' AND clocks.match_id = matches.id AND matches.opponent_id = clocks.user_id)"
  named_scope :recent, :order => 'matches.created_at DESC', :limit => 5
  named_scope :for, lambda { |user| {:conditions => ['user_id = ? OR opponent_id = ?', user.id, user.id]} }
  
  validates_presence_of :user_id, :opponent_id, :puzzle_id
  validate :self_challenges
  
  before_create :create_scrambles
  
  def finished?
    user.averages.match(id) != nil and opponent.averages.match(id) != nil
  end
  
  def pending?
    user.averages.match(id).nil? and opponent.averages.match(id).nil?
  end
  
  def challenged?
    user.averages.match(id) != nil and opponent.averages.match(id).nil?
  end
  
  def winner
    case averages.for(user.id) <=> averages.for(opponent.id)
    when -1
      user
    when 1
      opponent
    else
      nil
    end
  end
  
  def loser
    case averages.for(user.id) <=> averages.for(opponent.id)
    when -1
      opponent
    when 1
      user
    else
      nil
    end
  end
  
  def opponent_name_for(u)
    if u.id == user_id
      opponent.name
    else
      user.name
    end
  end
  
  private
    def self_challenges
      errors.add_to_base "You can't challenge yourself!" if user_id == opponent_id
    end
    
    def create_scrambles
      new_scrambles = puzzle.scrambles
      new_scrambles.each_with_index do |scramble, i|
        scrambles << Scramble.new(:scramble => scramble, :position => i)
      end
    end
end