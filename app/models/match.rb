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
  
  named_scope :finished, :conditions => "matches.status = 'finished'"
  named_scope :challenged, :conditions => "matches.status = 'challenged'"
  named_scope :recent, :order => 'matches.created_at DESC', :limit => 5
  named_scope :for, lambda { |user| {:conditions => ['user_id = ? OR opponent_id = ?', user.id, user.id]} }
  
  validates_presence_of :user_id, :opponent_id, :puzzle_id
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