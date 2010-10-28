class AddAmountToAverageRecord < ActiveRecord::Migration
  def self.up
    add_column :average_records, :amount, :integer, :null => false, :default => 5
    rename_table :average_records, :records
  end

  def self.down
    rename_table :records, :average_records
    remove_column :average_records, :amount
  end
end
