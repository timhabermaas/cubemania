class CreateScrambles < ActiveRecord::Migration
  def self.up
    create_table :scrambles do |t|
      t.string :scrambles, :null => false
      t.integer :competition_id, :null => false
      t.timestamps
    end
  end

  def self.down
    drop_table :scrambles
  end
end
