class RemovePaperclipFromKindsAndAddCssPosition < ActiveRecord::Migration
  def up
    remove_column :kinds, :image_file_name
    remove_column :kinds, :image_content_type
    remove_column :kinds, :image_file_size
    remove_column :kinds, :image_updated_at
    add_column :kinds, :css_position, :integer, :null => false, :default => 0
  end

  def down
    remove_column :kinds, :css_position
    add_column :kinds, :image_file_name, :string
    add_column :kinds, :image_content_type, :string
    add_column :kinds, :image_file_size, :integer
    add_column :kinds, :image_updated_at, :datetime
  end
end
