class FixDefaultsAndNullForSinglesColumns < ActiveRecord::Migration
  def up
    change_column :singles, :created_at, :datetime, :null => false, :default => DateTime.new(2013, 1, 1)
    change_column_default :singles, :created_at, nil
    change_column_default :singles, :user_id, nil
  end

  def down
    change_column_default :singles, :user_id, 0
    change_column :singles, :created_at, :datetime, :null => true
  end
end
