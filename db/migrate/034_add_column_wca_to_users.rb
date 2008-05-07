class AddColumnWcaToUsers < ActiveRecord::Migration
  def self.up
    add_column :users, :wca, :string
  end

  def self.down
    remove_column :users, :wca
  end
end
