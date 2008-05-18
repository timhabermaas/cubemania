class CreateParticipations < ActiveRecord::Migration
  def self.up
    create_table :participations do |t|
      t.integer :competition_id
      t.integer :user_id
      t.datetime :created_at
    end
  end

  def self.down
    drop_table :participations
  end
end
