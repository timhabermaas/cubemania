class CreateRecords < ActiveRecord::Migration
  def change
    create_table :records do |t|
      t.integer :time, :null => false
      t.integer :puzzle_id, :null => false
      t.integer :user_id, :null => false
      t.integer :amount, :null => false
      t.datetime :set_at, :null => false

      t.timestamps
    end
  end
end
