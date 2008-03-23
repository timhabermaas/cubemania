class CreateClocks < ActiveRecord::Migration
  def self.up
    create_table :clocks do |t|
      t.integer :time, :puzzle_id, :null => false
      t.datetime :created_at
    end
  end

  def self.down
    drop_table :clocks
  end
end
