class FixDefaultsAndNullForUsersColumns < ActiveRecord::Migration
  def up
    change_column :users, :created_at, :datetime, :null => false, :default => DateTime.new(2013, 1, 1)
    change_column :users, :updated_at, :datetime, :null => false, :default => DateTime.new(2013, 1, 1)
    change_column_default :users, :created_at, nil
    change_column_default :users, :updated_at, nil
  end

  def down
    change_column :users, :created_at, :datetime, :null => true
    change_column :users, :updated_at, :datetime, :null => true
  end
end
