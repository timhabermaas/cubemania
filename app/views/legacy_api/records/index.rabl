collection @records
attributes :id, :time, :set_at, :comment, :amount, :puzzle_id, :type_full_name

child :singles do
  attributes :id, :time, :scramble
end
