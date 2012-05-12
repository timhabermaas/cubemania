class CreateIndexesOnUsers < ActiveRecord::Migration
  def change
    add_index :users, :singles_count, :name => "index_users_on_singles_count"
    add_index :users, :email, :name => "index_users_on_email", :unique => true
    add_index :users, [:name, :encrypted_password], :name => "index_users_on_name_and_encrypted_password", :unique => true
  end
end
