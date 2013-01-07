class Cubemania.Models.Puzzle extends Backbone.Model

  initialize: ->
    @kind = new Cubemania.Models.Kind(@get("kind"))

  getId: ->
    @get("id")

  getName: ->
    @get("name")

  getFullName: ->
    @get("name") + " " + @kind.get("name")
