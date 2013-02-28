class AddCommentsCountToActivities < ActiveRecord::Migration
  def change
    add_column :activities, :comments_count, :integer, :null => false, :default => 0
  end
end
