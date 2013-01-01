module UrlHelper
  def puzzle_timers_path(id = "3x3x3")
    "/puzzles/#{id}/timer"
  end

  def puzzle_records_path(id = "3x3x3")
    "/puzzles/#{id}/records"
  end
end
