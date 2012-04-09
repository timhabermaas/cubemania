window.Cubemania =
  Models: {}
  Collections: {}
  Views: {}
  Routers: {}
  init: ->
    new Cubemania.Routers.Users()
    Backbone.history.start(pushState: true)

$(document).ready ->
  Cubemania.init()
