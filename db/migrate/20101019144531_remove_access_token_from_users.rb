class RemoveAccessTokenFromUsers < ActiveRecord::Migration
  def self.up
    remove_column :users, :fb_access_token
  end

  def self.down
    add_column :users, :fb_access_token, :string,
  end
end
