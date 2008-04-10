class MoveAttemptCountFromRecordsToPuzzles < ActiveRecord::Migration
  def self.up
    remove_column :records, :attempt_count
    add_column :puzzles, :attempt_count, :integer, :null => false, :default => 1
  end

  def self.down
    add_column :records, :attempt_count, :integer, :null => false, :default => 1
    remove_column :puzzles, :attempt_count
  end
end
