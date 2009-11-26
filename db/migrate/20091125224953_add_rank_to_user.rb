class AddRankToUser < ActiveRecord::Migration
  def self.up
    add_column :users, :rank, :integer, :null => false, :default => 0
  end

  def self.down
    remove_column :users, :rank
  end
end
