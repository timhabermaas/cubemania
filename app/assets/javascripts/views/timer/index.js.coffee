class Cubemania.Views.TimerIndex extends Backbone.View
  template: JST["timer/index"]

  initialize: ->
    @collection.on("reset", @render, this)
    @collection.on("add", @prependSingle, this)
    @statsView = new Cubemania.Views.Stats(singles: @collection, records: new Cubemania.Collections.Records())
    @timerView = new Cubemania.Views.Timer(collection: @collection)
    $(document).keydown(@timerView.stopTimer)
    $(document).keyup(@timerView.startTimer)

  render: ->
    $(@el).html(@template(singles: @collection))

    @timerView.setElement($("#timer")).render()
    @statsView.setElement($("#stats")).render()

    @collection.each(@appendSingle)
    this

  prependSingle: (single) ->
    view = new Cubemania.Views.Single(model: single)
    @$("#singles ol").prepend(view.render().el)

  appendSingle: (single) ->
    view = new Cubemania.Views.Single(model: single)
    @$("#singles ol").append(view.render().el)
