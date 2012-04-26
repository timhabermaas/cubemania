collection @records
attributes :time, :set_at, :comment

if params[:user_id]
  child :singles do
    attributes :time, :scramble
  end
else
  child :user do
    attributes :name, :slug
  end
end