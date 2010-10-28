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
      Clock.where(:record => true).where(:type => "Average").find_each do |average|
        singles = Clock.where(:average_id => average.id).where(:type => "Single").order(:position).all
        singles.each do |single|
          single.update_attribute :comment, average.comment
        end
      end
    end
    ActiveRecord::Base.record_timestamps = true

    execute "DELETE FROM clocks WHERE type='Average'"
    remove_index :clocks, :name => "index_clocks_on_match_id_and_user_id"
    remove_index :clocks, :name => "index_clocks_on_user_id_and_record_and_type"
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
