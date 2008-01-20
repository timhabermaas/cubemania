class Puzzle < ActiveRecord::Base
  belongs_to :kind
  
  file_column :image
end
