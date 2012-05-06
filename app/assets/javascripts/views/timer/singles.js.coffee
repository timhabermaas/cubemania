class Cubemania.Views.Singles extends Cubemania.BaseView
  template: JST["timer/singles"]

  initialize: ->
    @bindTo @collection, "reset", @render, this
    @bindTo @collection, "add", @prependSingle, this

    @bindTo @collection, "add", @hideOrShowSuggestion, this
    @bindTo @collection, "remove", @hideOrShowSuggestion, this
    @bindTo @collection, "reset", @hideOrShowSuggestion, this

  render: ->
    $(@el).html(@template(singles: @collection))
    @collection.each(@prependSingle)
    this

  prependSingle: (single) =>
    view = new Cubemania.Views.Single(model: single)
    @$("ol").prepend(view.render().el)

  hideOrShowSuggestion: (single) =>
    if @collection.models.length == 0
      @$("p.suggestion").show()
    else
      @$("p.suggestion").hide()
