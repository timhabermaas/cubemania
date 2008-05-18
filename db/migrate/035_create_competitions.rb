class CreateCompetitions < ActiveRecord::Migration
  def self.up
    create_table :competitions do |t|
      t.string :name, :null => false
      t.string :description
      t.integer :user_id
      t.timestamps
    end
  end

  def self.down
    drop_table :competitions
  end
end
