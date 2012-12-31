class RemoveCompetitions < ActiveRecord::Migration
  def up
    say_with_time "Rescuing average comments to singles" do
      execute "UPDATE singles SET comment = (SELECT comment FROM averages WHERE averages.id=singles.average_id) WHERE singles.average_id IS NOT NULL"
    end
    drop_table :competitions
    drop_table :averages
    drop_table :scrambles
    drop_table :shouts
    remove_column :singles, :average_id
    remove_column :users, :averages_count
  end

  def down
    raise ActiveRecord::IrreversibleMigration
  end
end
