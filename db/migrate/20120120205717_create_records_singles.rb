class CreateRecordsSingles < ActiveRecord::Migration
  def up
    create_table :records_singles, :id => false do |t|
      t.integer :record_id, :null => false
      t.integer :single_id, :null => false
    end
    remove_column :records, :singles
  end

  def down
    add_column :records, :singles, :text
    drop_table :records_singles
  end
end
