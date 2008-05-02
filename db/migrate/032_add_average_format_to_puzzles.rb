class AddAverageFormatToPuzzles < ActiveRecord::Migration
  def self.up
    add_column :puzzles, :average_format, :string, :null => false, :default => 'average'
  end

  def self.down
    remove_column :puzzles, :average_format
  end
end
