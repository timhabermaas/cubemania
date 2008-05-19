class CreatePuzzleipations < ActiveRecord::Migration
  def self.up
    create_table :puzzleipations do |t|
      t.integer :puzzle_id, :null => false
      t.integer :competition_id, :null => false
    end
  end

  def self.down
    drop_table :puzzleipations
  end
end
