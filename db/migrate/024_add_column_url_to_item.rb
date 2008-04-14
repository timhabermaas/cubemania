class AddColumnUrlToItem < ActiveRecord::Migration
  def self.up
    add_column :items, :url, :string, :limit => 32, :null => false, :default => ''
  end

  def self.down
    remove_column :items, :url
  end
end
