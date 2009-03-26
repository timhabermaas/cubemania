class ChangeEmailLimit < ActiveRecord::Migration
  def self.up
    change_column :users, :email, :string, :limit => 64
  end

  def self.down
    change_column :users, :email, :string, :limit => 32
  end
end
