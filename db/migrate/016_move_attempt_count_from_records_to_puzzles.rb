class MoveAttemptCountFromRecordsToPuzzles < ActiveRecord::Migration
  def self.up
    add_column :puzzles, :attempt_count, :integer, :null => false, :default => 1
  end

  def self.down
    remove_column :puzzles, :attempt_count
  end
end
