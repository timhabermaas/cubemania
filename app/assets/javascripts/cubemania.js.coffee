window.Cubemania =
  Models: {}
  Collections: {}
  Views: {}
  Routers: {}

  init: ->
    Cubemania.scrambler = new Cubemania.Scrambler()
    Cubemania.kinds = new Cubemania.Collections.Kinds($("#subnavigation").data("kinds"))
    Cubemania.puzzles = Cubemania.kinds.puzzles()

    Cubemania.currentPuzzle = new Cubemania.Models.CurrentPuzzle()
    Cubemania.currentUser = $("#user-profile").data("user-data")

    Cubemania.subnavigationView = new Cubemania.Views.Subnavigation(collection: Cubemania.kinds)
    Cubemania.subnavigationView.setElement($("#subnavigation")).render()

    new Cubemania.Routers.Router()
    Backbone.history.start(pushState: true)

    # fetch all links and use backbone to navigate
    if Backbone.history && Backbone.history._hasPushState
      $("a[rel=routing]").live "click", (event) ->
        href = $(this).attr("href")
        protocol = this.protocol + "//"

        if href.slice(protocol.length) != protocol
          event.preventDefault()
          Backbone.history.navigate(href, true)

$(document).ready ->
  Cubemania.init()
