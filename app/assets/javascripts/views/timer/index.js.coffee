class Cubemania.Views.TimerIndex extends Backbone.View
  template: JST["timer/index"]

  events:
    "click #timer a.toggle": "toggleManual"
    "submit #new_single": "createSingle"

  initialize: ->
    @collection.on("reset", @render, this)
    @collection.on("add", @prependSingle, this)
    @statsView = new Cubemania.Views.Stats(singles: @collection, records: new Cubemania.Collections.Records())

  render: ->
    $(@el).html(@template(singles: @collection))
    @$("#porno").append(@statsView.render().el)
    @collection.each(@appendSingle)
    this

  prependSingle: (single) ->
    view = new Cubemania.Views.Single(model: single)
    @$("#singles ol").prepend(view.render().el)

  appendSingle: (single) ->
    view = new Cubemania.Views.Single(model: single)
    @$("#singles ol").append(view.render().el)

  toggleManual: (event) ->
    event.preventDefault()
    $("#timer #new_single").toggle()
    $("#timer .time").toggle()
    a = event.currentTarget
    $(a).toggleText("Changed your mind?", "Set times manually")
    if $(a).text() == "Changed your mind?"
      $("#single_human_time").focus()
    else
      $("#single_human_time").blur()

  createSingle: (event) ->
    event.preventDefault()
    console.log $("#single_human_time").val()
    @collection.addSingle({time: $("#single_human_time").val()})
