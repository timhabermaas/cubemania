class AddIndexKindNameToPuzzles < ActiveRecord::Migration
  def self.up
    remove_index :puzzles, :kind_id
    add_index :puzzles, [:kind_id, :name], :unique => true
  end

  def self.down
    add_index :puzzles, :kind_id
    remove_index :puzzles, [:kind_id, :name]
  end
end