class MakeCommentsPolymorphic < ActiveRecord::Migration
  def up
    add_column :comments, :commentable_id, :integer, :null => false, :default => 0
    add_column :comments, :commentable_type, :string, :null => false, :default => ""
    change_column_default :comments, :commentable_id, nil
    change_column_default :comments, :commentable_type, nil
  end

  def down
    remove_column :comments, :commentable_id
    remove_column :comments, :commentable_type
  end
end
