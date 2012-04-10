class Cubemania.Routers.Users extends Backbone.Router
  routes:
    "users": "index"
    "users/:id": "show"

  initialize: ->
    @collection = new Cubemania.Collections.Users()
    @collection.reset($("#backbone-container").data("users"))

  index: ->
    view = new Cubemania.Views.UsersIndex(collection: @collection)
    $("#backbone-container").html(view.render().el)

  show: (id) ->
    model = new Cubemania.Models.User(id: id)
    model.fetch()
    view = new Cubemania.Views.UsersShow(model: model)
    $("#backbone-container").html(view.render().el)
