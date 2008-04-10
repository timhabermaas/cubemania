class CombineAveragesAndSinglesInClocks < ActiveRecord::Migration
  def self.up
    add_column :clocks, :type, :string, :null => false, :default => 'Single'
    add_column :clocks, :average_id, :integer
  end

  def self.down
    remove_column :clocks, :type
    remove_column :clocks, :average_id
  end
end