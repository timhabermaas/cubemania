class Cubemania.Routers.Router extends Backbone.Router
  routes:
    "": ""
    "puzzles/:puzzle/timer": "timerIndex"
    "puzzles/:puzzle/records": "recordsIndex"
    "users": "usersIndex"
    "users/:id": "usersShow"

  initialize: ->
    @bind "all", (router, route) ->
      unless router[6..-1] == "timerIndex"
        $(document).unbind("keydown")
        $(document).unbind("keyup")

  home: ->

  timerIndex: (puzzle_id) ->
    $(document).unbind("keydown")
    $(document).unbind("keyup")
    singles = new Cubemania.Collections.Singles(puzzle_id)
    singles.fetch(data: $.param({user_id: "tim"}))
    view = new Cubemania.Views.TimerIndex(collection: singles)
    $("#backbone-container").html(view.render().el)

  recordsIndex: (puzzle_id) ->
    records = new Cubemania.Collections.Records(puzzle_id)
    records.fetch()
    view = new Cubemania.Views.RecordsIndex(collection: records)
    $("#backbone-container").html(view.render().el)

  usersIndex: ->
    users = new Cubemania.Collections.Users()
    users.fetch()
    view = new Cubemania.Views.UsersIndex(collection: users)
    $("#backbone-container").html(view.render().el)

  usersShow: (id) ->
    model = new Cubemania.Models.User(id: id)
    model.fetch(async:false)
    view = new Cubemania.Views.UsersShow(model: model)
    $("#backbone-container").html(view.render().el)