class Cubemania.Models.User extends Backbone.Model

  urlRoot: "/users"

  activity: (max) ->
    if max == 0
      1
    else
      @get("singles_count") / max
