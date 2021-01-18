class Item < Struct.new(:name, :path, :exception)
  def selected?(params)
    name.pluralize.downcase == params[:controller] and
      not exception?(params)
  end

private
  def exception?(params)
    exception.call(params) unless exception.nil?
  end
end

module NavigationHelper
  def items
    @items ||=
      [
        Item.new("Home", root_path),
        Item.new("Timer", puzzle_timers_path_with_default),
        Item.new("Users", users_path, lambda { |p| p[:action] == "show" }),
        Item.new("Records", puzzle_records_path_with_default)
      ]
  end
end
