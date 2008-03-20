class AddIndexKindNameToPuzzles < ActiveRecord::Migration
  def self.up
    remove_index :puzzles, :kind_id
    add_index :puzzles, [:kind_id, :name]
  end

  def self.down
  end
end
