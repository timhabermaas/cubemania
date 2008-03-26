class ClockBelongsToUser < ActiveRecord::Migration
  def self.up
    add_column :clocks, :user_id, :integer, :null => false, :default => 0
  end

  def self.down
    remove_column :clocks, :user_id
  end
end