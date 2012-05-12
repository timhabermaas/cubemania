class AddCreatedAtAndUpdatedAtToPuzzles < ActiveRecord::Migration
  def up
    add_column :puzzles, :created_at, :datetime, :null => false, :default => Time.now
    change_column_default :puzzles, :created_at, nil
    add_column :puzzles, :updated_at, :datetime, :null => false, :default => Time.now
    change_column_default :puzzles, :updated_at, nil
  end

  def down
    remove_column :puzzles, :updated_at
    remove_column :puzzles, :created_at
  end
end
