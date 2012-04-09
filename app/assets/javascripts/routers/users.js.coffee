class Cubemania.Routers.Users extends Backbone.Router
  routes:
    "users": "index"

  initialize: ->
    @collection = new Cubemania.Collections.Users()
    @collection.fetch()

  index: ->
    view = new Cubemania.Views.UsersIndex(collection: @collection)
    $("#inner-content").html(view.render().el)
