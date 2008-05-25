class AddColumnStickyToCompetitions < ActiveRecord::Migration
  def self.up
    add_column :competitions, :sticky, :boolean
  end

  def self.down
    remove_column :competitions, :sticky
  end
end
