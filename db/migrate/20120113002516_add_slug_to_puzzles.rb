class AddSlugToPuzzles < ActiveRecord::Migration
  def change
    add_column :puzzles, :slug, :string
    add_index :puzzles, :slug, :unique => true
  end
end
