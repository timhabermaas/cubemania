window.Cubemania = # TODO is timer only! => Rename.
  Models: {}
  Presenters: {}
  Collections: {}
  Views: {}
  Routers: {}

  init: ->
    Cubemania.scrambler = new Cubemania.Scrambler()
    Cubemania.kinds = new Cubemania.Collections.Kinds($("#subnavigation").data("kinds"))
    Cubemania.puzzles = Cubemania.kinds.puzzles()

    Cubemania.currentPuzzle = new Cubemania.Models.CurrentPuzzle()
    Cubemania.currentUser = new Cubemania.Models.User($("#backbone-container").data("user-data"))

    Cubemania.viewManager = new Cubemania.ViewManager()

    Cubemania.flashView = new Cubemania.Views.Flash()
    Cubemania.flashView.setElement($("#flash"))

    Cubemania.loadingView = new Cubemania.Views.LoadingIndicator()

    new Cubemania.Routers.Router()
    Backbone.history.start(pushState: true)
