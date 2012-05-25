module UrlHelper
  def puzzle_timers_path(id = "3x3x3")
    "/puzzles/#{id}/timer"
  end

  def puzzle_records_path(id = "3x3x3")
    "/puzzles/#{id}/records"
  end

  def user_path(user)
    "/users/#{user.slug}"
  end

  def users_path
    "/users"
  end
end
