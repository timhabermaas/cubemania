class Item < ActiveRecord::Base
  validates_presence_of :name, :description
  validates_length_of :name, :maximum => 64
  validates_length_of :description, :maximum => 256
  
  def url
    { :controller => name }
  end
end
