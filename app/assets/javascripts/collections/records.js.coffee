class Cubemania.Collections.Records extends Backbone.Collection
  model: Cubemania.Models.Record

  url: ->
    "/puzzles/" + @puzzleId + "/records"

  initialize: (models, options) ->
    if options?
      @puzzleId = options.puzzleId
      @localStorage = new Backbone.LocalStorage("records-#{options.puzzleId}") if options.useLocalStorage

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
