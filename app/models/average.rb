class Average < ActiveRecord::Base
  belongs_to :user
  belongs_to :puzzle
  belongs_to :competition, :counter_cache => true
  has_many :singles, :dependent => :nullify

  attr_protected :user_id

  validates_presence_of :time, :user_id, :puzzle_id, :competition_id
  validate :hasnt_competed_before

  before_validation :set_user_and_puzzle_for_singles, :calculate_average

  accepts_nested_attributes_for :singles

  def comment
    ""
  end

  private
  def set_user_and_puzzle_for_singles
    singles.each do |s|
      s.user_id = self.user_id
      s.puzzle_id = self.puzzle_id
    end
  end

  def calculate_average
    return true if singles.any? { |s| s.time.nil? }
    average = CubingAverage.new(singles)
    self.time = average.time
    self.dnf = average.dnf?
    true # fuuuuuu has to return something trueish...
  end

  def hasnt_competed_before
    if competition && user && competition.participated?(Time.zone.now, user)
      errors.add(:user_id, "has already competed in this competition")
    end
  end
end
