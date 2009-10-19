class AddControllerAndActionToItem < ActiveRecord::Migration
  def self.up
    add_column :items, :controller, :string, :null => false, :default => 'homes', :limit => 64
    add_column :items, :action, :string, :null => false, :default => 'index', :limit => 32
    remove_column :items, :url
  end

  def self.down
    remove_column :items, :controller
    remove_column :items, :action
    add_column :items, :url, :string, :null => false, :default => '/'
  end
end
