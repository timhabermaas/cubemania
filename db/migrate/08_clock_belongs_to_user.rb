class ClockBelongsToUser < ActiveRecord::Migration
  def self.up
    add_column :clocks, :user_id, :integer, :null => false, :default => 0
    add_index :clocks, [:user_id, :puzzle_id, :created_at], :unique => true
  end

  def self.down
    remove_column :clocks, :user_id
    remove_index :clocks, [:user_id, :puzzle_id, :created_at]
  end
end