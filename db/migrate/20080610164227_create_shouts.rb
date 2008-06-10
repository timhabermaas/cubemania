class CreateShouts < ActiveRecord::Migration
  def self.up
    create_table :shouts do |t|
      t.string :content, :null => false
      t.integer :competition_id, :null => false
      t.integer :user_id, :null => false
      t.datetime :created_at
    end
  end

  def self.down
    drop_table :shouts
  end
end
