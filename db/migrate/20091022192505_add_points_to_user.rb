class AddPointsToUser < ActiveRecord::Migration
  def self.up
    add_column :users, :points, :integer, :null => false, :default => 1000
  end

  def self.down
    remove_column :users, :points
  end
end
