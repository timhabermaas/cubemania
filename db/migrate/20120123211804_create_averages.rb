class CreateAverages < ActiveRecord::Migration
  def change
    create_table :averages do |t|
      t.integer :time, :null => false
      t.boolean :dnf, :null => false, :default => false
      t.integer :user_id, :null => false
      t.integer :puzzle_id, :null => false
      t.integer :competition_id, :null => false

      t.timestamps
    end
  end
end
