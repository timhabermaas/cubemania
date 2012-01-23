class Average < ActiveRecord::Base
  has_and_belongs_to_many :singles
  belongs_to :user
  belongs_to :puzzle
  belongs_to :competition

  validates_presence_of :time, :user_id, :puzzle_id, :competition_id

  before_validation :set_user_and_puzzle_for_singles, :calculate_time

  accepts_nested_attributes_for :singles

  private
  def set_user_and_puzzle_for_singles
    singles.each do |s|
      s.user_id = self.user_id
      s.puzzle_id = self.puzzle_id
    end
  end

  def calculate_time
    self.time = CubingAverage.new(singles).time
  end
end
