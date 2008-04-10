class AddColumnCountdownToPuzzles < ActiveRecord::Migration
  def self.up
    add_column :puzzles, :countdown, :integer, :null => false, :default => 15
  end

  def self.down
    remove_column :puzzles, :countdown
  end
end
