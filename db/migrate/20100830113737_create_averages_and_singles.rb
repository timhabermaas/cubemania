class CreateAveragesAndSingles < ActiveRecord::Migration
  class Clock < ActiveRecord::Base
    Clock.inheritance_column = "Blub"
    belongs_to :user
    belongs_to :puzzle
  end

  class Average < ActiveRecord::Base
  end

  def self.up
    create_table :averages do |t|
      t.integer :time, :null => false
      t.boolean :dnf, :null => false, :default => false
      t.integer :user_id, :null => false
      t.integer :puzzle_id, :null => false
      t.integer :competition_id, :null => false
      t.string  :comment

      t.timestamps
    end

    say_with_time "Create competition averages from clocks" do
      ActiveRecord::Base.record_timestamps = false

      Clock.where(:type => "Average").where("competition_id IS NOT NULL").find_each do |clock|
        Average.create!(:time => clock.time,
                        :dnf => clock.dnf,
                        :user_id => clock.user_id,
                        :puzzle_id => clock.puzzle_id,
                        :competition_id => clock.competition_id,
                        :comment => clock.comment,
                        :created_at => clock.created_at, :updated_at => clock.created_at)
      end

      ActiveRecord::Base.record_timestamps = true
    end

    say_with_time "Rescuing average comments for non-competitions over to singles" do
      execute "UPDATE clocks SET comment = c2.comment FROM clocks c2 WHERE clocks.type='Single' AND clocks.competition_id IS NULL AND clocks.average_id=c2.id"
    end

    say_with_time "Get rid of all old averages in clocks" do
      execute "DELETE FROM clocks WHERE type='Average'"
    end

    remove_column :clocks, :type
    remove_column :clocks, :record
    remove_column :clocks, :position
    remove_column :clocks, :competition_id
    rename_table :clocks, :singles
    add_column :singles, :updated_at, :datetime, :null => false, :default => 0

    Single.reset_column_information
    ActiveRecord::Base.record_timestamps = false
    say_with_time "setting singles#updated_at to singles#created_at" do
      Single.update_all("updated_at = created_at")
    end
    ActiveRecord::Base.record_timestamps = true
    change_column :singles, :updated_at, :datetime, :null => false
  end

  def self.down
    raise ActiveRecord::IrreversibleMigration
  end
end
