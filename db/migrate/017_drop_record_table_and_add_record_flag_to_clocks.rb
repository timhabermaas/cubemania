class DropRecordTableAndAddRecordFlagToClocks < ActiveRecord::Migration
  def self.up
    drop_table :records
    add_column :clocks, :record, :boolean, :null => false, :default => false
  end

  def self.down
    remove_column :clocks, :record
  end
end