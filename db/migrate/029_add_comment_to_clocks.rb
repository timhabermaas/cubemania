class AddCommentToClocks < ActiveRecord::Migration
  def self.up
    add_column :clocks, :comment, :string
  end

  def self.down
    remove_column :clocks, :comment
  end
end
