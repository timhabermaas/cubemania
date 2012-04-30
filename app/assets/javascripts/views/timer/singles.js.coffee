class Cubemania.Views.Singles extends Cubemania.BaseView
  template: JST["timer/singles"]

  initialize: ->
    @bindTo @collection, "reset", @render, this
    @bindTo @collection, "add", @prependSingle, this

  render: ->
    $(@el).html(@template(singles: @collection))
    @collection.each(@prependSingle)
    this

  prependSingle: (single) =>
    view = new Cubemania.Views.Single(model: single)
    @$("ol").prepend(view.render().el)
