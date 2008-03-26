class Puzzle < ActiveRecord::Base
  belongs_to :kind, :order => :name
  
  file_column :image, :store_dir => 'public/images/puzzles', :base_url => 'images/puzzles'
  
  validates_presence_of :name, :image, :kind_id
  validates_length_of :name, :maximum => 64
  validates_filesize_of :image, :in => 0..10.kilobytes
  validates_file_format_of :image, :in => ['gif']
end