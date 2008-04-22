class Kind < ActiveRecord::Base
  has_many :puzzles, :order => 'name', :dependent => :destroy
  
  file_column :image, :store_dir => 'public/images/kinds', :base_url => 'images/kinds'
  
  validates_presence_of :name
  validates_length_of :name, :maximum => 64
  validates_filesize_of :image, :in => 0..10.kilobytes
  validates_file_format_of :image, :in => ['gif', 'png']
  
  def self.all
    find :all, :include => :puzzles
  end
end