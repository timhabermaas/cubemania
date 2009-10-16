class Match < ActiveRecord::Base
  belongs_to :puzzle
  belongs_to :user; attr_protected :user_id, :user
  belongs_to :opponent, :class_name => 'User', :foreign_key => 'opponent_id'
  
  validates_presence_of :user_id
  validates_presence_of :opponent_id
  
  validate :self_challenge
  
  
  def pending?
    user.averages.find(:first, :conditions => { :match_id => id }).nil? and opponent.averages.find(:first, :conditions => { :match_id => id }).nil?
  end
  
  def challenged?
    not user.averages.find(:first, :conditions => { :match_id => id }).nil? and opponent.averages.find(:first, :conditions => { :match_id => id }).nil?
  end
  
  def finished?
    not user.averages.find(:first, :conditions => { :match_id => id }).nil? and not opponent.averages.find(:first, :conditions => { :match_id => id }).nil?
  end
  
  private
    def self_challenge
      errors.add_to_base "You can't challenge yourself" unless user_id != opponent_id
    end
end