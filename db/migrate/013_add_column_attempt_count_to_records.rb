class AddColumnAttemptCountToRecords < ActiveRecord::Migration
  def self.up
    add_column :records, :attempt_count, :integer, :null => false, :default => 1
  end

  def self.down
    remove_column :records, :attempt_count
  end
end