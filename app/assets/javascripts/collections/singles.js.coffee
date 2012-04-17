class Cubemania.Collections.Singles extends Backbone.Collection
  model: Cubemania.Models.Single

  url: ->
      "/puzzles/" + @puzzle + "/singles"

  initialize: (puzzle) ->
    @puzzle = puzzle

  addSingle: (attributes) ->
    s = new Cubemania.Models.Single()
    s.set(attributes)
    s.set("puzzle_id", @puzzle)
    s.save()
    @unshift(s) # TODO order automatically

  currentAverage: (size) ->
    lastSingles = @models[0..size]
    dnfs = _.filter(lastSingles, (s) -> s.dnf())
    return null if dnfs.length > 1 or lastSingles.length < size

    solvedSingles = _.reject(lastSingles, (s) -> s.dnf())
    times = _.map(solvedSingles, (s) -> s.get("time"))
    sum = _.reduce(times, ((memo, t) -> memo + t), 0)

    if dnfs.length == 0
      (sum - _.min(times) - _.max(times)) / (size - 2)
    else
      (sum - _.min(times)) / (size - 1)
