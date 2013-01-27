class RemoveDefaultFromSinglesUpdatedAt < ActiveRecord::Migration
  def up
    change_column_default :singles, :updated_at, nil
  end

  def down
    change_column_default :singles, :updated_at, DateTime.new(2010, 1, 1)
  end
end
