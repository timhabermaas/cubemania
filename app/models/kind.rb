class Kind < ActiveRecord::Base
  has_many :puzzles, :order => :dimension
end
