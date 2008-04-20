class AddIndexes < ActiveRecord::Migration
  def self.up
    add_index :posts, :created_at
    add_index :items, :position
    add_index :users, :name, :unique => true
    add_index :users, :email, :unique => true
    add_index :clocks, [:puzzle_id, :record, :type, :time]
    add_index :clocks, [:user_id, :record, :type]
    remove_index :clocks, [:user_id, :puzzle_id, :created_at]
    add_index :clocks, [:user_id, :puzzle_id, :type, :created_at]
    remove_column :users, :clocks_count
    add_column :users, :averages_count, :integer, :null => false, :default => 0
    add_index :users, :averages_count
  end

  def self.down
    remove_index :posts, :created_at
    remove_index :items, :position
    remove_index :users, :name
    remove_index :users, :email
    remove_index :clocks, [:puzzle_id, :record, :type, :time]
    remove_index :clocks, [:user_id, :record, :type]
    add_index :clocks, [:user_id, :puzzle_id, :created_at]
    remove_index :clocks, [:user_id, :puzzle_id, :type, :created_at]
    add_column :users, :clocks_count, :integer, :null => false, :default => 0
    remove_index :users, :averages_count
    remove_column :users, :averages_count
  end
end