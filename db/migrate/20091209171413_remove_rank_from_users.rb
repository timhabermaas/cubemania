class RemoveRankFromUsers < ActiveRecord::Migration
  def self.up
    remove_column :users, :rank
  end

  def self.down
    add_column :users, :rank, :integer, :null => false, :default => 1000
  end
end
