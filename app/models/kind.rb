class Kind < ActiveRecord::Base
  has_many :puzzles, :order => 'name', :dependent => :destroy
  
  has_attached_file :image, :path => ":rails_root/public/images/kinds/:id/:style/:basename.:extension",
                            :url => "/images/kinds/:id/:style/:basename.:extension"
  
  validates_presence_of :name
  validates_length_of :name, :maximum => 64
  
  def self.all
    find :all, :include => :puzzles
  end
end