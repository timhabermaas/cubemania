class AddVersionToPuzzles < ActiveRecord::Migration
  def self.up
    add_column :puzzles, :version, :integer, :default => 0
  end

  def self.down
    remove_column :puzzles, :version
  end
end
