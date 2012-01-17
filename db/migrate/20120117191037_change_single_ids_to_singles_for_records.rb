class ChangeSingleIdsToSinglesForRecords < ActiveRecord::Migration
  def change
    remove_column :records, :single_ids
    add_column :records, :singles, :text
  end
end
