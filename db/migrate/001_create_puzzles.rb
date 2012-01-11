class CreatePuzzles < ActiveRecord::Migration
  def self.up
    create_table :puzzles do |t|
      t.string :name, :image, :limit => 64, :null => false
      t.integer :kind_id, :null => false
    end
  end

  def self.down
    drop_table :puzzles
  end
end