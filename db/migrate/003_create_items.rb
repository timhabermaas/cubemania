class CreateItems < ActiveRecord::Migration
  def self.up
    create_table :items do |t|
      t.string :name, :limit => 64, :null => false
      t.string :description, :null => false
    end
  end

  def self.down
    drop_table :items
  end
end
