class Cubemania.Views.Flash extends Backbone.View

  events:
    "click .close": "clickClose"

  initialize: ->
    @message = ""

  show: (message) ->
    @message = message
    $(@el).show()
    @render()

  hide: ->
    @message = ""
    $(@el).hide()

  slideDown: (message) ->
    @message = message
    @render()
    $(@el).slideDown("fast")

  slideUp: ->
    $(@el).slideUp("fast")

  clickClose: (event) ->
    event.preventDefault()
    @slideUp()

  render: ->
    $(@el).html("<p>#{@message}</p><a href='#' class='close'>Close</a>")
    this
