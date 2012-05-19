class Cubemania.Views.LoadingIndicator extends Backbone.View

  initialize: ->
    unless $("#loading").length > 0
      $("body").append("<div id='loading'>Loading...</div>")
    $("#loading").ajaxStart(@startedLoading)
    $("#loading").ajaxStop(@stoppedLoading)

  startedLoading: =>
    @timeoutInterval = setTimeout(@show, 1000)

  show: ->
    $("#loading").show()

  stoppedLoading: (m) =>
    if @timeoutInterval?
      clearTimeout(@timeoutInterval)
      @timeoutInterval = null
    $("#loading").hide()
