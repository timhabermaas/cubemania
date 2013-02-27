class MigrateOldCommentsData < ActiveRecord::Migration
  def up
    Comment.update_all("commentable_id = post_id")
    Comment.update_all("commentable_type = 'Post'")
    remove_column :comments, :post_id
  end

  def down
    add_column :comments, :post_id, :integer, :null => false, :default => 0
    change_column_default :comments, :post_id, nil
    Comment.where(:commentable_type => "Post").update_all("post_id = commentable_id")
    Comment.where("commentable_type <> 'Post'").delete_all
  end
end
