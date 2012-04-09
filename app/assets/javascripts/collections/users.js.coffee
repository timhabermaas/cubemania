class Cubemania.Collections.Users extends Backbone.Collection
  url: "/users"
  model: Cubemania.Models.User

  maxSinglesCount: ->
    user = _.max(this.models, (u) -> u.get("singles_count"))
    if user
      user.get("singles_count")
    else
      0