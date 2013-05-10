class AddSingleIdsToRecords < ActiveRecord::Migration
  def change
    add_column :records, :single_ids, :text
  end
end
