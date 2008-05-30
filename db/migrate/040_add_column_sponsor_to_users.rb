class AddColumnSponsorToUsers < ActiveRecord::Migration
  def self.up
    add_column :users, :sponsor, :boolean, :null => false, :default => false
  end

  def self.down
    remove_column :users, :sponsor
  end
end
