class CreateRecordsSingles < ActiveRecord::Migration
  def change
    create_table :records_singles, :id => false do |t|
      t.integer :record_id, :null => false
      t.integer :single_id, :null => false
    end
  end
end
