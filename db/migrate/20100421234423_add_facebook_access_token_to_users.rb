class AddFacebookAccessTokenToUsers < ActiveRecord::Migration
  def self.up
    add_column :users, :fb_access_token, :string
    remove_column :users, :twitter_secret
    remove_column :users, :twitter_token
  end

  def self.down
    remove_column :users, :fb_access_token
    add_column :users, :twitter_secret, :string
    add_column :users, :twitter_token, :string
  end
end
