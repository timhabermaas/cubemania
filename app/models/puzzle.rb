class Puzzle < ActiveRecord::Base
  belongs_to :kind, :order => :name
  
  file_column :image
  
  validates_presence_of :name, :image, :kind_id
  validates_length_of :name, :maximum => 64
end
