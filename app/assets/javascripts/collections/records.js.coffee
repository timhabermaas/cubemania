class Cubemania.Collections.Records extends Backbone.Collection
  model: Cubemania.Models.Record

  url: ->
    "/puzzles/" + @puzzleId + "/records?type=" + @type

  initialize: (puzzleId, type) ->
    @setPuzzleId puzzleId
    @type = type || "avg5"

  setPuzzleId: (puzzleId) ->
    @puzzleId = puzzleId
