class CreateRecords < ActiveRecord::Migration
  def self.up
    create_table :records do |t|
      t.integer :time, :puzzle_id, :user_id, :clock_id, :null => false
    end
    add_index :records, [:puzzle_id, :time]
    add_index :records, :user_id
  end

  def self.down
    drop_table :records
    remove_index :records, [:puzzle_id, :time]
    remove_index :records, :user_id
  end
end