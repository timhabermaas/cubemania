class CreateCompetitions < ActiveRecord::Migration
  def self.up
    create_table :competitions do |t|

      t.timestamps
    end
  end

  def self.down
    drop_table :competitions
  end
end
