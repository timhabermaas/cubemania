class Kind < ActiveRecord::Base
  has_many :puzzles, :order => 'name', :dependent => :destroy

  validates_presence_of :name
  validates_length_of :name, :maximum => 64
  validates_format_of :css_class, :with => /^[a-zA-Z]{1,20}$/

  def self.all
    find :all, :include => :puzzles
  end
end
