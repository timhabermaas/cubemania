class AddUniqueIndexConstraintToRecords < ActiveRecord::Migration
  def up
    records = execute "SELECT * FROM records"
    # duplicated_records :: Hash (PuzzleId, Amount, UserId) [RecordId]
    duplicated_records = Hash.new { |h, k| h[k] = [] }
    records.each do |record|
      duplicated_records[[record.fetch("puzzle_id"), record.fetch("amount"), record.fetch("user_id")]] << record.fetch("id")
    end
    # Removing all non-unique records, keeping the newest one.
    duplicated_records = duplicated_records.select { |k, v| v.size > 1 }.map { |k, v| v - [v.max] }.flatten
    execute "DELETE FROM records WHERE id IN (#{duplicated_records.join(",")})"
    execute "DELETE FROM records_singles WHERE record_id IN (#{duplicated_records.join(",")})"
    add_index :records, [:puzzle_id, :amount, :user_id], unique: true, name: "index_unique_records_on_puzzle_id_amount_user_id"
  end

  def down
    remove_index "index_unique_records_on_puzzle_id_amount_user_id"
  end
end
