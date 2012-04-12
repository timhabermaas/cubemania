class Cubemania.Collections.Users extends Backbone.Collection
  model: Cubemania.Models.User
  url: "/users"

  maxSinglesCount: ->
    user = _.max(this.models, (u) -> u.get("singles_count"))
    if user
      user.get("singles_count")
    else
      0