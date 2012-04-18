window.Cubemania =
  Models: {}
  Collections: {}
  Views: {}
  Routers: {}
  currentUserId: ->
    parseInt $("#user-profile").data("user-id")

  init: ->
    Cubemania.scrambler = new Cubemania.Scrambler()

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
