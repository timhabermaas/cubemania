class Cubemania.Routers.Router extends Backbone.Router
  routes:
    "": "home"
    "puzzles/:puzzle_id/timer": "timerIndex"
    "puzzles/:puzzle_id/records": "recordsIndex"
    "users": "usersIndex"
    "users/:id": "usersShow"

  initialize: ->
    @bind "all", @cleanupKeybindings
    @bind "all", @showOrHideSubnavigation

  home: ->

  timerIndex: (puzzle_id) ->
    singles = new Cubemania.Collections.Singles(puzzle_id)
    singles.fetch(data: $.param({user_id: Cubemania.currentUser.get("id")}))
    view = new Cubemania.Views.TimerIndex(collection: singles)
    Cubemania.viewManager.changeView(view)

    Cubemania.currentPuzzle.on("change", @updateRoute, this)
    Cubemania.currentPuzzle.set(Cubemania.puzzles.findByIdOrSlug(puzzle_id))
    $(document).unbind("keydown")
    $(document).unbind("keyup")

    $("#backbone-container").html(view.render().el)

  recordsIndex: (puzzle_id) ->
    records = new Cubemania.Collections.Records(puzzle_id)
    records.fetch()
    view = new Cubemania.Views.RecordsIndex(collection: records)
    Cubemania.viewManager.changeView(view)

    Cubemania.currentPuzzle.on("change", @updateRoute, this)
    Cubemania.currentPuzzle.set(Cubemania.puzzles.findByIdOrSlug(puzzle_id))

    $("#backbone-container").html(view.render().el)

  usersIndex: ->
    users = new Cubemania.Collections.Users()
    users.fetch()
    view = new Cubemania.Views.UsersIndex(collection: users)
    Cubemania.viewManager.changeView(view)
    $("#backbone-container").html(view.render().el)

  usersShow: (id) ->
    model = new Cubemania.Models.User(id: id)
    model.fetch(async:false)
    view = new Cubemania.Views.UsersShow(model: model)
    Cubemania.viewManager.changeView(view)
    $("#backbone-container").html(view.render().el)

  cleanupKeybindings: (router, route) ->
    unless router[6..-1] == "timerIndex"
      $(document).unbind("keydown")
      $(document).unbind("keyup")

  showOrHideSubnavigation: (router, route) ->
    switch router[6..-1]
      when "timerIndex", "recordsIndex"
        Cubemania.subnavigationView.show()
      else
        Cubemania.subnavigationView.hide()

  updateRoute: (puzzle) ->
    route = Backbone.history.fragment
    if route[0..6] == "puzzles" or route[0..7] == "/puzzles"
      route = route.replace(/puzzles\/[^/]*\//,"puzzles/#{puzzle.get("slug")}/")
      Backbone.history.navigate(route)
