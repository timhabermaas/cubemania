window.Cubemania =
  Models: {}
  Collections: {}
  Views: {}
  Routers: {}
  init: ->
    new Cubemania.Routers.Users()
    Backbone.history.start(pushState: true)

    # fetch all links and use backbone to navigate
    if Backbone.history && Backbone.history._hasPushState
      $("#backbone-container").delegate "a", "click", (event) ->
        href = $(this).attr("href")
        protocol = this.protocol + "//"

        if href.slice(protocol.length) != protocol
          event.preventDefault()
          Backbone.history.navigate(href, true)

$(document).ready ->
  Cubemania.init()
