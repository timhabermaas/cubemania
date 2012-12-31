module UrlHelper
  def puzzle_timers_path(id = "3x3x3")
    "/puzzles/#{id}/timer"
  end

  def puzzle_records_path(id = "3x3x3", type = "avg5")
    "/puzzles/#{id}/records?type=#{type}"
  end
end
