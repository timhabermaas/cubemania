class AddColumnClocksCountToUsers < ActiveRecord::Migration
  def self.up
    add_column :users, :clocks_count, :integer, :null => false, :default => 0
  end

  def self.down
    remove_column :users, :clocks_count
  end
end
