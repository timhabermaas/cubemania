class Cubemania.Routers.Router extends Backbone.Router
  routes:
    "": ""
    "puzzles/:puzzle/timer": "timerIndex"
    "puzzles/:puzzle/records": "recordsIndex"
    "users": "usersIndex"
    "users/:id": "usersShow"

  home: ->

  timerIndex: (puzzle) ->

  recordsIndex: (puzzle) ->
    collection = new Cubemania.Collections.Records(puzzle)
    collection.fetch()
    view = new Cubemania.Views.RecordsIndex(collection: collection)
    $("#backbone-container").html(view.render().el)

  usersIndex: ->
    collection = new Cubemania.Collections.Users()
    collection.fetch()
    view = new Cubemania.Views.UsersIndex(collection: collection)
    $("#backbone-container").html(view.render().el)

  usersShow: (id) ->
    model = new Cubemania.Models.User(id: id)
    model.fetch(async:false)
    view = new Cubemania.Views.UsersShow(model: model)
    $("#backbone-container").html(view.render().el)