class CreateCubingSessions < ActiveRecord::Migration
  def change
    create_table :cubing_sessions do |t|
      t.integer :user_id
      t.integer :puzzle_id
      t.text :single_ids
      t.timestamps
    end
  end
end
