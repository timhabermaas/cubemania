class Cubemania.Routers.Router extends Backbone.Router
  routes:
    "puzzles/:puzzle_id/timer": "timerIndex"

  timerIndex: (puzzle_id) ->
    Cubemania.currentPuzzle.set(Cubemania.puzzles.findByIdOrSlug(puzzle_id), false)

    singles = new Cubemania.Collections.Singles([], puzzleId: puzzle_id, useLocalStorage: !Cubemania.currentUser.present())
    singles.fetch(data: $.param(user_id: Cubemania.currentUser.get("id")))
    records = new Cubemania.Collections.Records([], puzzleId: puzzle_id, useLocalStorage: !Cubemania.currentUser.present())
    records.fetch(data: $.param(user_id: Cubemania.currentUser.get("id")))

    Cubemania.subnavigationView.makeAutoHideable()

    view = new Cubemania.Views.TimerIndex(collection: singles, records: records)

    $("#backbone-container").html(view.render().el)
