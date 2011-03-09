class Match < ActiveRecord::Base
  extend ActiveSupport::Memoizable

  STATUSES = %w(pending challenged finished)
  C1 = 30
  C2 = 400

  #attr_protected :user, :user_id, :status
  attr_accessible :opponent_id, :opponent, :puzzle_id, :puzzle

  belongs_to :puzzle
  belongs_to :user
  belongs_to :opponent, :class_name => 'User'
  has_many :averages, :dependent => :nullify do
    def for(user_id)
      find_by_user_id(user_id)
    end
  end
  has_many :scrambles, :as => :matchable, :order => 'position', :dependent => :delete_all

  scope :finished, :conditions => "matches.status = 'finished'", :order => 'matches.updated_at DESC'
  scope :challenged, :conditions => "matches.status = 'challenged'", :order => 'matches.updated_at DESC'
  scope :recent, :order => 'matches.updated_at DESC', :limit => 5
  scope :for, lambda { |user| {:conditions => ['user_id = ? OR opponent_id = ?', user.id, user.id], :order => 'matches.updated_at DESC'} }

  validates_presence_of :user_id, :opponent_id, :puzzle_id
  validates_inclusion_of :status, :in => STATUSES
  validate :self_challenges

  before_create :create_scrambles
  before_destroy :undo_points, :if => lambda { |m| m.user_points != nil and m.opponent_points != nil }

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
      self.status = 'finished'
    elsif user.averages.match(id).nil? and opponent.averages.match(id).nil?
      self.status = 'pending'
    else
      self.status = 'challenged'
    end
    save
  end

  def winner
    if finished? and not averages.for(user.id) == averages.for(opponent.id)
      averages.for(user.id) < averages.for(opponent.id) ? user : opponent
    else
      nil
    end
  end

  def loser
    if finished? and not averages.for(user.id) == averages.for(opponent.id)
      averages.for(user.id) > averages.for(opponent.id) ? user : opponent
    else
      nil
    end
  end
  memoize :winner, :loser

  def other(u)
    u.id == user_id ? opponent : user
  end

  def points_for(u)
    u.id == user_id ? user_points : opponent_points
  end

  def max_win(user)
    user == self.user ? (C1 * (1 - expectation)).round : (C1 * expectation).round
  end

  def max_loss(user)
    user == self.user ? (-expectation * C1).round : ((expectation - 1) * C1).round
  end

  def update_points
    if finished? and self.user_points.nil? and self.opponent_points.nil?
      user_win = 1 - averages.for(user.id).ratio(averages.for(opponent.id))
      self.user_points = ((user_win - expectation) * C1).round
      self.opponent_points = (((1 - user_win) - (1 - expectation)) * C1).round
      save
      user.update_attribute :points, user.points + self.user_points
      opponent.update_attribute :points, opponent.points + self.opponent_points
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

    def undo_points
      user.update_attribute :points, user.points - user_points
      opponent.update_attribute :points, opponent.points - opponent_points
    end

    def expectation
      1.0/(1 + 10 ** ((opponent.points - user.points)/C2.to_f))
    end
end