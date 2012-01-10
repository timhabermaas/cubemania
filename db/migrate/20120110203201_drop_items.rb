class DropItems < ActiveRecord::Migration
  def up
    drop_table :items
  end

  def down
    create_table :items do |t|
      t.string  :name, :limit => 64, :null => false
      t.string  :description, :null => false
      t.integer :position, :default => 0, :null => false
      t.string  :controller, :limit => 64, :default => "homes", :null => false
      t.string  :action, :limit => 32, :default => "index", :null => false
    end
  end
end
