class AddUserPointsOpponentPointsToMatch < ActiveRecord::Migration
  def self.up
    add_column :matches, :user_points, :integer
    add_column :matches, :opponent_points, :integer
  end

  def self.down
    remove_column :matches, :opponent_points
    remove_column :matches, :user_points
  end
end
