class ChangeUserIdToNullableInShouts < ActiveRecord::Migration
  def self.up
    change_column :shouts, :user_id, :integer, :null => true
  end

  def self.down
    change_column :shouts, :user_id, :integer, :null => false
  end
end
