class AddImageToKinds < ActiveRecord::Migration
  def self.up
    add_column :kinds, :image, :string, :limit => 64
  end

  def self.down
    remove_column :kinds, :image
  end
end