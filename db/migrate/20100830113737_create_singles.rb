class CreateSingles < ActiveRecord::Migration
  class Clock < ActiveRecord::Base
  end 
  class AverageRecord < ActiveRecord::Base
    validates_presence_of :puzzle_id, :user_id
    validates_uniqueness_of :user_id, :scope => :puzzle_id
  end

  def self.up
    create_table :average_records do |t|
      t.integer :time, :null => false
      t.integer :puzzle_id, :null => false
      t.integer :user_id, :null => false

      t.timestamps
    end
    AverageRecord.reset_column_information

    say_with_time "Coping old average records over to new table" do
      Clock.where(:record => true).where(:type => "Average").each do |average|
        record = AverageRecord.new :time => average.time, :puzzle_id => average.puzzle_id, :user_id => average.user_id
        record.save!
      end
    end

    execute "DELETE FROM clocks WHERE type='Average'"
    remove_index :clocks, :name => "index_clocks_on_match_id_and_user_id"
    remove_index :clocks, :name => "index_clocks_on_user_id_and_record_and_type"
    remove_column :clocks, :comment
    remove_column :clocks, :type
    remove_column :clocks, :average_id
    remove_column :clocks, :record
    remove_column :clocks, :position
    remove_column :clocks, :competition_id
    remove_column :clocks, :match_id
    rename_table :clocks, :singles
  end

  def self.down
    raise ActiveRecord::IrreversibleMigration
  end
end
