class Cubemania.Collections.Records extends Backbone.Collection
  url: ->
    "/puzzles/" + @puzzle + "/records?type=" + @type

  initialize: (puzzle, type) ->
    @puzzle = puzzle
    @type = type || "avg5"

  model: Cubemania.Models.Record
