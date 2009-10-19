class CreateMatches < ActiveRecord::Migration
  def self.up
    create_table :matches do |t|
      t.integer :user_id, :null => false
      t.integer :opponent_id, :null => false
      t.integer :puzzle_id, :null => false
      t.timestamps
    end
  end

  def self.down
    drop_table :matches
  end
end
