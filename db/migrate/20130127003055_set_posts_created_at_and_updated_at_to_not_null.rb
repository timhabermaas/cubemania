class SetPostsCreatedAtAndUpdatedAtToNotNull < ActiveRecord::Migration
  def up
    change_column :posts, :created_at, :datetime, :null => false, :default => DateTime.new(2013, 1, 1)
    change_column :posts, :updated_at, :datetime, :null => false, :default => DateTime.new(2013, 1, 1)
    change_column_default :posts, :created_at, nil
    change_column_default :posts, :updated_at, nil
  end

  def down
    change_column :posts, :created_at, :datetime, :null => true
    change_column :posts, :updated_at, :datetime, :null => true
  end
end
