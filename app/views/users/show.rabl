object @user
attributes :id, :name, :wasted_time, :slug

child :records do
  attributes :time, :amount, :set_at, :puzzle_id, :comment
  child :puzzle do
    attributes :id, :name, :css_position
    child :kind do
      attributes :id, :name, :short_name, :css_position
    end
  end
end