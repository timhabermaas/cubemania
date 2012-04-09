class Cubemania.Models.User extends Backbone.Model

  activity: (max) ->
    if max == 0
      1
    else
      @get("singles_count") / max
