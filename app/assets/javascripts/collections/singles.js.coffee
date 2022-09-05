class Cubemania.Collections.Singles extends Backbone.Collection
  model: Cubemania.Models.Single

  url: ->
    "/legacy_api/puzzles/" + @puzzleId + "/singles"

  comparator: (single) ->
    single.get("created_at")

  initialize: (models, options) ->
    @setPuzzleId(options.puzzleId)
    @localStorage = new Backbone.LocalStorage("singles-#{options.puzzleId}") if options.useLocalStorage

  setPuzzleId: (puzzleId) ->
    @puzzleId = puzzleId
    @localStorage = new Backbone.LocalStorage("singles-#{puzzleId}") if @localStorage?

  currentAverage: (size) ->
    lastSingles = @recent(size)
    dnfs = _.filter(lastSingles, (s) -> s.dnf())
    return null if dnfs.length > 1 or lastSingles.length < size

    solvedSingles = _.reject(lastSingles, (s) -> s.dnf())
    times = _.map(solvedSingles, (s) -> s.get("time"))
    sum = _.reduce(times, ((memo, t) -> memo + t), 0)

    if dnfs.length == 0
      (sum - _.min(times) - _.max(times)) / (size - 2)
    else
      (sum - _.min(times)) / (size - 2)

  currentMean: (size) ->
    lastSingles = @recent(size)
    dnfs = _.filter(lastSingles, (s) -> s.dnf())
    return null if dnfs.length > 0 or lastSingles.length < size

    times = _.map(lastSingles, (s) -> s.get("time"))
    sum = _.reduce(times, ((memo, t) -> memo + t), 0)
    sum / size

  recent: (amount) =>
    @models[-amount..-1]

  today: =>
    today = new Date()
    today.setDate(today.getDate() - 1)
    _.filter(@models, (s) -> new Date(s.get("created_at")) > today)

  best: ->
    _.min(@models, (s) -> s.get("time"))

  worst: ->
    _.max(@models, (s) -> s.get("time"))

  lastSingle: ->
    @models[-1..-1][0]
