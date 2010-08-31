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
      t.string :singles_string, :null => false, :limit => 256

      t.timestamps
    end
    AverageRecord.reset_column_information
    
    ActiveRecord::Base.record_timestamps = false
    say_with_time "Copying old average records over to new table" do
      Clock.where(:record => true).where(:type => "Average").find_each do |average|
        singles = Clock.where(:average_id => average.id).where(:type => "Single").order(:position).all
        record = AverageRecord.new :time => average.time,
                                   :puzzle_id => average.puzzle_id,
                                   :user_id => average.user_id,
                                   :created_at => average.created_at,
                                   :updated_at => average.created_at,
                                   :singles => singles
        unless singles.empty?
          record.save!
        end
      end
    end
    ActiveRecord::Base.record_timestamps = true

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
