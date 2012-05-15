class Cubemania.Routers.Router extends Backbone.Router
  routes:
    "puzzles/:puzzle_id/timer": "timerIndex"
    "puzzles/:puzzle_id/records": "recordsIndex" # TODO add type in url in order to not lose it when changing puzzles
    "users": "usersIndex"
    "users/:id": "usersShow"

  initialize: ->
    @bind "all", @showOrHideSubnavigation
    @bind "all", @_trackPageview
    Cubemania.currentPuzzle.on("change", @updateRoute, this)

  timerIndex: (puzzle_id) ->
    Cubemania.currentPuzzle.set(Cubemania.puzzles.findByIdOrSlug(puzzle_id), false)

    singles = new Cubemania.Collections.Singles([], puzzleId: puzzle_id, useLocalStorage: !Cubemania.currentUser.present())
    singles.fetch(data: $.param(user_id: Cubemania.currentUser.get("id")))
    records = new Cubemania.Collections.Records([], puzzleId: puzzle_id, useLocalStorage: !Cubemania.currentUser.present())
    records.fetch(data: $.param(user_id: Cubemania.currentUser.get("id")))

    view = new Cubemania.Views.TimerIndex(collection: singles, records: records)
    Cubemania.viewManager.changeView(view)

    unless Cubemania.currentUser.present()
      Cubemania.flashView.show("You're currently not logged in!<br /> <a href='/login'>Login</a> or <a href='/register'>register</a> to save your times permanently.")

    $("#backbone-container").html(view.render().el)

  recordsIndex: (puzzle_id) ->
    Cubemania.currentPuzzle.set(Cubemania.puzzles.findByIdOrSlug(puzzle_id), false)

    records = new Cubemania.Collections.Records([], puzzleId: puzzle_id)
    records.fetch()
    view = new Cubemania.Views.RecordsIndex(collection: records)
    Cubemania.viewManager.changeView(view)

    $("#backbone-container").html(view.render().el)

  usersIndex: ->
    users = new Cubemania.Collections.Users()
    users.fetch()
    view = new Cubemania.Views.UsersIndex(collection: users)
    Cubemania.viewManager.changeView(view)
    $("#backbone-container").html(view.render().el)

  usersShow: (id) ->
    model = new Cubemania.Models.User(id: id)
    model.fetch()
    view = new Cubemania.Views.UsersShow(model: model)
    Cubemania.viewManager.changeView(view)
    $("#backbone-container").html(view.el)

  showOrHideSubnavigation: (router, route) ->
    switch router[6..-1]
      when "timerIndex"
        Cubemania.subnavigationView.show()
        Cubemania.subnavigationView.checkPuzzleAndKind(Cubemania.currentPuzzle.puzzle)
        Cubemania.subnavigationView.makeAutoHideable()
      when "recordsIndex"
        Cubemania.subnavigationView.show()
        Cubemania.subnavigationView.checkPuzzleAndKind(Cubemania.currentPuzzle.puzzle)
        Cubemania.subnavigationView.unmakeAutoHideable()
      else
        Cubemania.subnavigationView.hide()

  updateRoute: (puzzle) ->
    route = Backbone.history.fragment
    if route[0..6] == "puzzles" or route[0..7] == "/puzzles"
      route = route.replace(/puzzles\/[^/]*\//,"puzzles/#{puzzle.get("slug")}/")
      Backbone.history.navigate(route, true)

  _trackPageview: ->
    url = Backbone.history.fragment
    _gaq.push(["_trackPageview", "/#{url}"])
