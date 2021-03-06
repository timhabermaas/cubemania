class RemoveDefaultsFromCompetitions < ActiveRecord::Migration
  def up
    change_column :competitions, :puzzle_id, :integer, :null => false
    change_column_default :competitions, :puzzle_id, nil
  end

  def down
    change_column :competitions, :puzzle_id, :integer, :null => false, :default => 0
  end
end
