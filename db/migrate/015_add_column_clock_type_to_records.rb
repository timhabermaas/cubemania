class AddColumnClockTypeToRecords < ActiveRecord::Migration
  def self.up
    add_column :records, :clock_type, :string, :null => false, :default => 'Single'
  end

  def self.down
    remove_column :records, :clock_type
  end
end