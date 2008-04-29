class AllowUserIdOfCommentsToBeNull < ActiveRecord::Migration
  def self.up
    change_column :comments, :user_id, :integer, :null => true
  end

  def self.down
    change_column :comments, :user_id, :integer, :null => false
  end
end