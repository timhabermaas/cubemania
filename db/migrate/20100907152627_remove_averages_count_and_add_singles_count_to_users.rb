class RemoveAveragesCountAndAddSinglesCountToUsers < ActiveRecord::Migration
  def self.up
    add_column :users, :singles_count, :integer, :default => 0, :null => false
    say_with_time "Update users.singles_count" do
      User.find_each do |u|
        singles_count = execute("SELECT COUNT(*) as count FROM singles WHERE user_id=#{u.id}")
        execute "UPDATE users SET singles_count=#{singles_count[0]["count"]} WHERE id=#{u.id}"
      end
    end
  end

  def self.down
    remove_column :users, :singles_count
  end
end
