module UrlHelper
  def puzzle_timers_path_with_default(id = "3x3x3")
    "/puzzles/#{id}/timer"
  end

  def puzzle_records_path_with_default(id = "3x3x3")
    "/puzzles/#{id}/records"
  end

  def subnavigation_path(puzzle)
    url_for :puzzle_id => puzzle.slug, :controller => params[:controller]
  end

  def csv_download_path(user_id, puzzle_id)
    payload = { :download_csv_for => current_user.id, :exp => Time.now.to_i + 30 * 3600 }
    token = JWT.encode payload, ENV.fetch("HMAC_SECRET"), "HS256"
    "/api/singles.csv?user_id=#{user_id}&puzzle_id=#{puzzle_id}&token=#{token}"
  end
end
