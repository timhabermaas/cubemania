class RemoveRecordIdFromPuzzles < ActiveRecord::Migration
  def up
    remove_column :puzzles, :record_id
  end

  def down
    add_column :puzzles, :record_id, :integer
  end
end
