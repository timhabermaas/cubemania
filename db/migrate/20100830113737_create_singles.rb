class CreateSingles < ActiveRecord::Migration
  class Clock < ActiveRecord::Base
    Clock.inheritance_column = "Blub"
    belongs_to :user
    belongs_to :puzzle
  end

  def self.up
    create_table :average_records do |t|
      t.integer :time, :null => false
      t.integer :puzzle_id, :null => false
      t.integer :user_id, :null => false
      t.string :single_ids, :null => false, :limit => 256

      t.timestamps
    end

    ActiveRecord::Base.record_timestamps = false
    say_with_time "Rescuing average comments over to singles" do
      Clock.find_by_sql("UPDATE clocks AS c1 INNER JOIN clocks AS c2 ON c1.average_id=c2.id SET c1.comment = c2.comment WHERE c1.type='Single'")
    end
    ActiveRecord::Base.record_timestamps = true

    execute "DELETE FROM clocks WHERE type='Average'"
    remove_column :clocks, :type
    remove_column :clocks, :average_id
    remove_column :clocks, :record
    remove_column :clocks, :position
    remove_column :clocks, :competition_id
    rename_table :clocks, :singles
  end

  def self.down
    raise ActiveRecord::IrreversibleMigration
  end
end
