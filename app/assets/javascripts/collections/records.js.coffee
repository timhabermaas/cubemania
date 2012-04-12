class Cubemania.Collections.Records extends Backbone.Collection
  model: Cubemania.Models.Record

  url: ->
    "/puzzles/" + @puzzle + "/records?type=" + @type

  initialize: (puzzle, type) ->
    @puzzle = puzzle
    @type = type || "avg5"


