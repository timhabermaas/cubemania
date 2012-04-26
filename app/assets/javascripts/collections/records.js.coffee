class Cubemania.Collections.Records extends Backbone.Collection
  model: Cubemania.Models.Record

  url: ->
    "/puzzles/" + @puzzleId + "/records"

  initialize: (models, options) ->
    @puzzleId = options.puzzleId if options?

  setPuzzleId: (puzzleId) ->
    @puzzleId = puzzleId

  getSingleRecord: ->
    @getByAmount(1)

  getAvg5Record: ->
    @getByAmount(5)

  getAvg12Record: ->
    @getByAmount(12)

  getByAmount: (amount) ->
    _.find(@models, (r) -> r.get("amount") == amount)
