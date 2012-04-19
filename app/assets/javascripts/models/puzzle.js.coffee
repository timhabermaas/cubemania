class Cubemania.Models.Puzzle extends Backbone.Model

  initialize: ->
    @kind = new Cubemania.Models.Kind(@get("kind"))
