class ChangeCompetitionsTable < ActiveRecord::Migration
  def self.up
    change_column :competitions, :name, :string, :limit => 64, :null => false
    add_column :competitions, :puzzle_id, :integer, :null => false, :default => 0
    add_column :competitions, :repeat, :string, :limit => 32, :null => false, :default => 'once'
  end

  def self.down
    remove_column :competitions, :puzzle_id
    remove_column :competitions, :repeat
  end
end