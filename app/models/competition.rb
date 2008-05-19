class Competition < ActiveRecord::Base
  belongs_to :user; attr_protected :user_id, :user
  has_many :participations
  has_many :users, :through => :participations
  has_many :puzzleipations
  has_many :puzzles, :through => :puzzleipations
  has_many :averages, :dependent => :nullify do
    def for(puzzle_id); find_all_by_puzzle_id puzzle_id, :order => 'time'; end
  end
  has_many :singles, :dependent => :nullify
  
  validates_uniqueness_of :name
  validates_presence_of :name
end