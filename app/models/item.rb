class Item < ActiveRecord::Base
  validates_presence_of :name, :url, :description
  validates_length_of :name, :maximum => 64
  validates_length_of :url, :maximum => 32
  validates_length_of :description, :maximum => 256
  
  def self.all
    find :all, :order => 'name'
  end
end