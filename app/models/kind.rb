class Kind < ActiveRecord::Base
  has_many :puzzles
  
  validates_presence_of :name
  validates_length_of :name, :maximum => 64
end
