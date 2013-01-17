module UsersHelper
  def count_to_color(count)
    case count
    when 0
      "#8BA4C0"
    when 1..50
      "#d6e685"
    when 51..100
      "#8cc665"
    when 101..200
      "#44a340"
    else
      "#1e6823"
    end
  end
end
