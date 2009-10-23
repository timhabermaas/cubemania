class Match < ActiveRecord::Base
  STATUSES = %w(pending challenged finished)
  C1 = 30
  C2 = 400
  
  belongs_to :puzzle
  belongs_to :user; attr_protected :user, :user_id
  belongs_to :opponent, :class_name => 'User'
  has_many :averages do
    def for(user_id)
      find_by_user_id(user_id)
    end
  end
  has_many :scrambles, :as => :matchable, :order => 'position'
  
  named_scope :finished, :conditions => "matches.status = 'finished'"
  named_scope :challenged, :conditions => "matches.status = 'challenged'"
  named_scope :recent, :order => 'matches.created_at DESC', :limit => 5
  named_scope :for, lambda { |user| {:conditions => ['user_id = ? OR opponent_id = ?', user.id, user.id]} }
  
  validates_presence_of :user_id, :opponent_id, :puzzle_id
  validates_inclusion_of :status, :in => STATUSES
  validate :self_challenges
  
  before_create :create_scrambles
  
  def finished?
    status == 'finished'
  end
  
  def pending?
    status == 'pending'
  end
  
  def challenged?
    status == 'challenged'
  end
  
  def update_status!
    if user.averages.match(id) != nil and opponent.averages.match(id) != nil
      update_attribute :status, 'finished'
    elsif user.averages.match(id).nil? and opponent.averages.match(id).nil?
      update_attribute :status, 'pending'
    else
      update_attribute :status, 'challenged'
    end
  end
  
  def winner
    if averages.for(user_id) and averages.for(opponent_id) and averages.for(user.id) != averages.for(opponent.id)
      [user, opponent].sort_by{ |u| u.averages.match(id) }.first
    end
  end
  
  def loser
    if averages.for(user_id) and averages.for(opponent_id) and averages.for(user.id) != averages.for(opponent.id)
      [user, opponent].sort_by{ |u| u.averages.match(id) }.last
    end
  end
  
  def opponent_name_for(u)
    u.id == user_id ? opponent.name : user.name
  end
  
  def max_win(user)
    user == self.user ? (C1 * (1 - expectation)).round : (C1 * expectation).round
  end
  
  def max_loss(user)
    user == self.user ? (-expectation * C1).round : ((expectation - 1) * C1).round
  end
  
  def update_points
    if finished?
      user_win = 1 - averages.for(user).ratio(averages.for(opponent))
      user.update_attribute :points, user.points + ((user_win - expectation) * C1).round
      opponent.update_attribute :points, opponent.points + (((1 - user_win) - expectation) * C1).round
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
    
    def expectation
      1.0/(1 + 10 ** ((opponent.points - user.points)/C2.to_f))
    end
end