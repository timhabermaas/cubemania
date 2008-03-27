class AddTurnsToPuzzles < ActiveRecord::Migration
  def self.up
    add_column :puzzles, :scramble_length, :integer
  end

  def self.down
    remove_column :puzzles, :scramble_length
  end
end
