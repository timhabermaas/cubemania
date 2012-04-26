collection @records
attributes :id, :time, :set_at, :comment, :amount, :puzzle_id

if params[:user_id]
  child :singles do
    attributes :id, :time, :scramble
  end
else
  child :user do
    attributes :id, :name, :slug
  end
end