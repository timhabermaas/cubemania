window.Cubemania = # TODO is timer only! => Rename.
  Models: {}
  Presenters: {}
  Collections: {}
  Views: {}
  Routers: {}

  init: ->
    Cubemania.scrambler = new Cubemania.Scrambler()

    Cubemania.currentPuzzle = new Cubemania.Models.Puzzle($("#backbone-container").data("puzzle"))
    Cubemania.currentUser = new Cubemania.Models.User($("#backbone-container").data("user-data"))

    Cubemania.flashView = new Cubemania.Views.Flash()
    Cubemania.flashView.setElement($("#flash"))

    Cubemania.loadingView = new Cubemania.Views.LoadingIndicator()

    puzzle_id = Cubemania.currentPuzzle.getId()

    singles = new Cubemania.Collections.Singles([], puzzleId: puzzle_id, useLocalStorage: !Cubemania.currentUser.present())
    singles.fetch(data: $.param(user_id: Cubemania.currentUser.getId()))
    records = new Cubemania.Collections.Records([], puzzleId: puzzle_id, useLocalStorage: !Cubemania.currentUser.present())
    records.fetch(data: $.param(user_id: Cubemania.currentUser.getId()))

    Cubemania.subnavigationView.makeAutoHideable()

    view = new Cubemania.Views.TimerIndex(collection: singles, records: records)

    $("#backbone-container").html(view.render().el)
