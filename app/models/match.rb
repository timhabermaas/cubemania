class Match < ActiveRecord::Base
  belongs_to :puzzle
  belongs_to :user; attr_protected :user, :user_id
  belongs_to :opponent, :class_name => 'User'
  has_many :averages
  has_many :scrambles, :as => :matchable, :order => 'position'
  
  named_scope :finished, :conditions => "EXISTS(SELECT clocks.* FROM clocks WHERE clocks.match_id = matches.id AND clocks.type = 'Average' AND matches.user_id = clocks.user_id) AND
            EXISTS(SELECT clocks.* FROM clocks WHERE clocks.type = 'Average' AND clocks.match_id = matches.id AND matches.opponent_id = clocks.user_id)"
  
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
  
  private
    def self_challenges
      errors.add_to_base "You can't challenge yourself!" if user_id == opponent_id
    end
    
    def create_scrambles
      new_scrambles = puzzle.scrambles
      Scramble.transaction do
        new_scrambles.each_with_index do |scramble, i|
          scrambles << Scramble.new(:scramble => scramble, :position => i)
        end
      end
    end
end