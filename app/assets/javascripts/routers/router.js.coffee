class Cubemania.Routers.Router extends Backbone.Router
  routes:
    "puzzles/:puzzle_id/timer": "timerIndex"

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

    view = Cubemania.viewManager.newView(Cubemania.Views.TimerIndex, {collection: singles, records: records})

    $("#backbone-container").html(view.render().el)

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
