class AddColumnAveragesCountToCompetitions < ActiveRecord::Migration
  def self.up
    add_column :competitions, :averages_count, :integer, :default => 0, :null => false
  end

  def self.down
    remove_column :competitions, :averages_count
  end
end
