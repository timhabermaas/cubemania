class UseBigintForWastedTime < ActiveRecord::Migration
  def up
    change_column :users, :wasted_time, :bigint, :default => 0, :null => false
  end

  def down
    change_column :users, :wasted_time, :integer, :default => 0, :null => false
  end
end
