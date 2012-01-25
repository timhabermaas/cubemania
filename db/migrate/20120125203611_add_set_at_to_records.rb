class AddSetAtToRecords < ActiveRecord::Migration
  def change
    add_column :records, :set_at, :datetime, :null => false, :default => Time.now
  end
end
