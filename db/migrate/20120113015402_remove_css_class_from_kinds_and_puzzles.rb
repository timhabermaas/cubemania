class RemoveCssClassFromKindsAndPuzzles < ActiveRecord::Migration
  def up
    remove_column :kinds, :css_class
    remove_column :puzzles, :css_class
  end

  def down
    add_column :kinds, :css_class, :string, :null => false, :default => "dummy"
    add_column :puzzles, :css_class, :string, :null => false, :default => "dummy"
  end
end
