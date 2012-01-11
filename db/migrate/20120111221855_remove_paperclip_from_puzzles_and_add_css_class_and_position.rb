class RemovePaperclipFromPuzzlesAndAddCssClassAndPosition < ActiveRecord::Migration
  def up
    remove_column :puzzles, :image_file_name
    remove_column :puzzles, :image_content_type
    remove_column :puzzles, :image_file_size
    remove_column :puzzles, :image_updated_at
    add_column :puzzles, :css_class, :string, :null => false, :default => "dummy"
    add_column :puzzles, :css_position, :integer, :null => false, :default => 0
  end

  def down
    remove_column :puzzles, :css_class
    remove_column :puzzles, :css_position
    add_column :puzzles, :image_file_name, :string
    add_column :puzzles, :image_content_type, :string
    add_column :puzzles, :image_file_size, :integer
    add_column :puzzles, :image_updated_at, :datetime
  end
end
