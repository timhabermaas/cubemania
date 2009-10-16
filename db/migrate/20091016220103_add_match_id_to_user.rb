class AddMatchIdToUser < ActiveRecord::Migration
  def self.up
    add_column :users, :match_id, :integer
  end

  def self.down
    remove_column :users, :match_id
  end
end
