class AddTwitterTokenAndTwitterSecretToUsers < ActiveRecord::Migration
  def self.up
    add_column :users, :twitter_token, :string
    add_column :users, :twitter_secret, :string
  end

  def self.down
    remove_column :users, :twitter_secret
    remove_column :users, :twitter_token
  end
end
