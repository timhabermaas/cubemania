class AddColumnRecordIdToPuzzles < ActiveRecord::Migration
  def self.up
    add_column :puzzles, :record_id, :integer
  end

  def self.down
    remove_column :puzzles, :record_id
  end
end
