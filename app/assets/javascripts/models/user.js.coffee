class Cubemania.Models.User extends Backbone.Model

  getId: ->
    @get("id")

  present: ->
    @get("id")?
