class ChangeLimitOfEncryptedPassword < ActiveRecord::Migration
  def self.up
    change_column :users, :encrypted_password, :string, :null => false, :limit => 255
  end

  def self.down
  end
end