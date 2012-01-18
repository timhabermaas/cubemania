class ChangeSingleIdsToSinglesForRecords < ActiveRecord::Migration
  def up
    remove_column :records, :single_ids
    add_column :records, :singles, :text
  end

  def down
    remove_column :records, :singles
    add_column :records, :single_ids, :string
  end
end
