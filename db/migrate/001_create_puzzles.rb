class CreatePuzzles < ActiveRecord::Migration
  def self.up
    create_table :puzzles do |t|
      t.string :name, :image, :limit => 42, :null => false
      t.integer :kind_id, :dimension, :null => false
      t.timestamps
    end
    add_index :puzzles, [:kind_id, :dimension]
  end

  def self.down
    drop_table :puzzles
    remove_index :puzzles, [:kind_id, :dimension]
  end
end
