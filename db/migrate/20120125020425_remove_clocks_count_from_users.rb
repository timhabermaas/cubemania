class RemoveClocksCountFromUsers < ActiveRecord::Migration
  def up
    remove_column :users, :clocks_count
  end

  def down
    add_column :users, :clocks_count, :integer, :null => false, :default => 0
  end
end
