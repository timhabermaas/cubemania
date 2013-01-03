module UrlHelper
  def puzzle_timers_path(id = "3x3x3")
    "/puzzles/#{id}/timer"
  end

  def puzzle_records_path(id = "3x3x3")
    "/puzzles/#{id}/records"
  end

  def subnavigation_path(puzzle)
    url_for :puzzle_id => puzzle.slug, :controller => params[:controller]
  end
end
