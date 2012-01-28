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
    Average.reset_column_information

    say_with_time "Create competition averages from clocks" do
      ActiveRecord::Base.record_timestamps = false

      Clock.where(:type => "Average").where("competition_id IS NOT NULL").find_each do |average| # total behämmert!
        a = Average.create!(:time => average.time,
                            :dnf => average.dnf,
                            :user_id => average.user_id,
                            :puzzle_id => average.puzzle_id,
                            :competition_id => average.competition_id,
                            :comment => average.comment,
                            :created_at => average.created_at, :updated_at => average.created_at)
        Clock.where(:type => "Single", :average_id => average.id).each do |single|
          single.update_attribute :average_id, a.id
        end
      end

      ActiveRecord::Base.record_timestamps = true
    end

    say_with_time "Rescuing average comments for non-competitions over to singles" do
      execute "CREATE TABLE comments_temp AS SELECT id, comment FROM clocks WHERE type='Average' AND competition_id IS NULL"
      execute "CREATE INDEX temp_index ON comments_temp (id)"
      execute "UPDATE clocks SET comment = (SELECT comment FROM comments_temp WHERE comments_temp.id=clocks.average_id) WHERE type='Single' AND competition_id IS NULL"
      execute "DROP TABLE comments_temp"
    end

    say_with_time "Set all average_ids to NULL for singles" do
      Clock.where(:type => "Single").where("competition_id IS NULL").update_all("average_id = NULL")
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
    say_with_time "setting singles#updated_at to singles#created_at" do
      Single.update_all("updated_at = created_at")
    end
    change_column :singles, :updated_at, :datetime, :null => false
  end

  def self.down
    raise ActiveRecord::IrreversibleMigration
  end
end
