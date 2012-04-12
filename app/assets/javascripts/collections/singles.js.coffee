class Cubemania.Collections.Singles extends Backbone.Collection
  model: Cubemania.Models.Single

  url: ->
      "/puzzles/" + @puzzle + "/singles"

  initialize: (puzzle) ->
    @puzzle = puzzle