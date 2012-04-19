class Cubemania.Models.Kind extends Backbone.Model

  initialize: ->
    @puzzles = new Cubemania.Collections.Puzzles(@get("puzzles"))
