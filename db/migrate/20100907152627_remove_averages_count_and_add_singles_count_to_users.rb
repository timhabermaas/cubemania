class RemoveAveragesCountAndAddSinglesCountToUsers < ActiveRecord::Migration
  def self.up
    add_column :users, :singles_count, :integer, :default => 0, :null => false
    remove_column :users, :averages_count
    User.update_all("singles_count = (Select count(*) from singles where users.id = singles.user_id)")
  end

  def self.down
    add_column :users, :averages_count, :integer, :default => 0, :null => false
    remove_column :users, :singles_count
  end
end
