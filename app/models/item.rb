class Item < ActiveRecord::Base
  validates_presence_of :name, :controller, :description, :position, :action
  validates_length_of :name, :maximum => 64
  validates_length_of :description, :maximum => 255
  validates_length_of :controller, :maximum => 64
  validates_length_of :action, :maximum => 32

  def self.all
    find :all, :order => 'position'
  end
end