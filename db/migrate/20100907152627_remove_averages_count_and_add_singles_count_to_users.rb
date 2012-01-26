class RemoveAveragesCountAndAddSinglesCountToUsers < ActiveRecord::Migration
  def self.up
    add_column :users, :singles_count, :integer, :default => 0, :null => false
    say_with_time "Update users.singles_count" do
      User.find_each do |u|
        puts "User ##{u.id}"
        u.update_attribute :singles_count, u.singles.count
      end
    end
  end

  def self.down
    remove_column :users, :singles_count
  end
end
