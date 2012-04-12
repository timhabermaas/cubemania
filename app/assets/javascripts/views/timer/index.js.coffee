class Cubemania.Views.TimerIndex extends Backbone.View
  template: JST["timer/index"]

  initialize: ->
    @collection.on("reset", @render, this)
    @statsView = new Cubemania.Views.Stats(singles: @collection, records: new Cubemania.Collections.Records())

  render: ->
    $(@el).html(@template(singles: @collection))
    @$("#porno").html(@statsView.render().el)
    @collection.each(@appendSingle)
    this

  appendSingle: (single) ->
    view = new Cubemania.Views.Single(model: single)
    @$("#singles ol").append(view.render().el)
