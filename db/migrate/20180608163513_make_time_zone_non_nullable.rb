class MakeTimeZoneNonNullable < ActiveRecord::Migration
  def up
    change_column_null :users, :time_zone, false, "UTC"
  end

  def down
    change_column_null :users, :time_zone, true, "UTC"
  end
end
