class AddColumnRoleToUser < ActiveRecord::Migration
  def self.up
    add_column :users, :role, :string, :limit => 16, :default => 'user'
  end

  def self.down
    remove_column :users, :role
  end
end