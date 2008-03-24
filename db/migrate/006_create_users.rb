class CreateUsers < ActiveRecord::Migration
  def self.up
    create_table :users do |t|
      t.string :name, :limit => 32, :null => false
      t.string :email, :limit => 32, :null => false
      t.string :salt, :limit => 8, :null => false
      t.string :encrypted_password, :limit => 16, :null => false
      t.datetime :created_at
    end
    add_index :users, [:name, :encrypted_password], :unique => true
  end

  def self.down
    drop_table :users
    remove_index :users, [:name, :encrypted_password]
  end
end