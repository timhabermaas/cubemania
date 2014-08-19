class RecordCalculationJob < Struct.new(:user_id, :puzzle_id)
  def exists?
    Delayed::Job.where(:handler => handler).where("failed_at IS NULL").count > 0
  end

  def handler
    self.to_yaml # trololol hack!
  end

  def perform
    UpdateRecords.for User.find(user_id), Puzzle.find(puzzle_id)
  end
end
