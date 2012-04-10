class Cubemania.Routers.Router extends Backbone.Router
  routes:
    "users": "index"
    "users/:id": "show"

  index: ->
    collection = new Cubemania.Collections.Users()
    collection.fetch()
    view = new Cubemania.Views.UsersIndex(collection: collection)
    $("#backbone-container").html(view.render().el)

  show: (id) ->
    model = new Cubemania.Models.User(id: id)
    model.fetch(async:false)
    view = new Cubemania.Views.UsersShow(model: model)
    $("#backbone-container").html(view.render().el)
