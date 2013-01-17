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

  def month_beginning_in_this_week(week_date) # TODO use day/month classes to avoid fragile comparisons
    current_month = week_date.beginning_of_month
    return current_month if week_date.day == 1
    7.times do |i|
      new_month = (week_date + i.days).beginning_of_month
      if current_month != new_month
        return new_month
      end
    end

    nil
  end
end
