class AddWastedTimeToUsers < ActiveRecord::Migration
  def change
    add_column :users, :wasted_time, :integer, :null => false, :default => 0
  end
end
