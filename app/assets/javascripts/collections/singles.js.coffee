class Cubemania.Collections.Singles extends Backbone.Collection
  model: Cubemania.Models.Single

  url: ->
    "/puzzles/" + @puzzleId + "/singles"

  comparator: (single) ->
    single.get("created_at")

  initialize: (puzzleId) ->
    @setPuzzleId(puzzleId)

  setPuzzleId: (puzzleId) ->
    @puzzleId = puzzleId

  currentAverage: (size) ->
    lastSingles = @models[-size..-1]
    dnfs = _.filter(lastSingles, (s) -> s.dnf())
    return null if dnfs.length > 1 or lastSingles.length < size

    solvedSingles = _.reject(lastSingles, (s) -> s.dnf())
    times = _.map(solvedSingles, (s) -> s.get("time"))
    sum = _.reduce(times, ((memo, t) -> memo + t), 0)

    if dnfs.length == 0
      (sum - _.min(times) - _.max(times)) / (size - 2)
    else
      (sum - _.min(times)) / (size - 1)