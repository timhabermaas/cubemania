class AddCompetitionIdToClocks < ActiveRecord::Migration
  def self.up
    add_column :clocks, :competition_id, :integer
  end

  def self.down
    remove_column :competition_id
  end
end
