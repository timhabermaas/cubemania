class RemoveAveragesCountAndAddSinglesCountToUsers < ActiveRecord::Migration
  def self.up
    add_column :users, :singles_count, :integer, :default => 0, :null => false
    say_with_time "Update users.singles_count" do
      User.find_by_sql "UPDATE users u SET singles_count = (SELECT COUNT(*) FROM singles s WHERE s.user_id = u.id)"
    end
  end

  def self.down
    remove_column :users, :singles_count
  end
end
