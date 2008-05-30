class CreateScrambles < ActiveRecord::Migration
  def self.up
    create_table :scrambles do |t|
      t.text :scrambles, :null => false
      t.integer :competition_id, :null => false
      t.datetime :created_at
    end
  end

  def self.down
    drop_table :scrambles
  end
end