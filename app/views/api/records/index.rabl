collection @records
attributes :id, :time, :set_at, :comment, :amount, :puzzle_id

child :singles do
  attributes :id, :time, :scramble
end
