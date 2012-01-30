class AddIndexOnSinglesAndRecords < ActiveRecord::Migration
  def change
    add_index :singles, [:user_id, :puzzle_id, :created_at], :name => "index_singles_on_user_id_and_puzzle_id_and_created_at"
    add_index :records, [:puzzle_id, :amount, :time], :name => "index_records_on_puzzle_id_and_amount_and_time"
  end
end
