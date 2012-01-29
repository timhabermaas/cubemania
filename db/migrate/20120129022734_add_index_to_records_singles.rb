class AddIndexToRecordsSingles < ActiveRecord::Migration
  def change
    add_index :records_singles, [:record_id, :single_id], :name => "index_records_singles_on_record_id_and_single_id"
  end
end
