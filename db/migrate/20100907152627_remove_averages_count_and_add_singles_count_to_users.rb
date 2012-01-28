class RemoveAveragesCountAndAddSinglesCountToUsers < ActiveRecord::Migration
  def self.up
    add_column :users, :singles_count, :integer, :default => 0, :null => false
    add_column :users, :slug, :string
    User.reset_column_information
    say_with_time "Update users.singles_count" do
      User.find_each do |user|
        singles_count = user.singles.count
        user.update_attribute :singles_count, singles_count
      end
    end
    remove_column :users, :slug
  end

  def self.down
    remove_column :users, :singles_count
  end
end
