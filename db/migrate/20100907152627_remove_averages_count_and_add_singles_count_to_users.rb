class RemoveAveragesCountAndAddSinglesCountToUsers < ActiveRecord::Migration
  def self.up
    add_column :users, :singles_count, :integer, :default => 0, :null => false
    say_with_time "Filling users table with singles_count" do
      User.find_each do |user|
        singles_count = user.singles.count
        puts "#{user.id}: #{user.name} (#{singles_count})"
        user.update_all(:singles_count => singles_count)
      end
    end
  end

  def self.down
    remove_column :users, :singles_count
  end
end
