class AddSkillToCompetition < ActiveRecord::Migration
  def self.up
    add_column :competitions, :skill, :string, :limit => 32, :null => false, :default => 'all'
  end

  def self.down
    remove_column :competitions, :skill
  end
end
