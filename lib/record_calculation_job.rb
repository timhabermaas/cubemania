class RecordCalculationJob < Struct.new(:single)
  def exists?
    Delayed::Job.where(:handler => handler).where("failed_at IS NULL").count > 0
  end

  def handler
    self.to_yaml # trololol hack!
  end

  # TODO add transaction?
  def perform
    InvalidateRecords.for(single)

    recreate_records_for_all_types(single)
  end

  private
  def recreate_records_for_all_types(single)
    RecordType.all.map do |type|
      singles = Single.younger_than(single).order("created_at")
      singles_2 = Single.last_n_prior_to(type.count, single).reverse

      all_singles = singles_2 + singles
      old_record = single.user.records.where(:puzzle_id => single.puzzle_id,
                                             :amount => type.count).
                                       recent.first
      RecalculateRecordsHistory.for!(type, all_singles, old_record)
    end.flatten
  end
end
