class AddIgnoreToUsers < ActiveRecord::Migration
  def self.up
    add_column :users, :ignore, :boolean, :default => false
  end

  def self.down
    remove_column :users, :ignore
  end
end
