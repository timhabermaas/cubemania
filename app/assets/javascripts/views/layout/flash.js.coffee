class Cubemania.Views.Flash extends Cubemania.BaseView

  events:
    "click .close": "clickClose"

  initialize: ->
    @message = ""
    @bindTo Cubemania.currentPuzzle, "change", @slideUp, this

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
