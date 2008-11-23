class RemoveFileColumn < ActiveRecord::Migration
  def self.up
    remove_column :puzzles, :image
    remove_column :kinds, :image
  end

  def self.down
    add_column :puzzles, :image, :string, :limit => 64, :null => false
    add_column :kinds, :image, :string, :limit => 64, :null => false
  end
end
