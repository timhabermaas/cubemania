class Cubemania.Views.Singles extends Cubemania.BaseView
  template: JST["timer/singles"]

  events:
    "click .load-more a": "loadMore"

  initialize: ->
    @bindTo @collection, "reset", @render, this
    @bindTo @collection, "add", @prependSingle, this

    @bindTo @collection, "add", @hideOrShowSuggestion, this
    @bindTo @collection, "remove", @hideOrShowSuggestion, this
    @bindTo @collection, "reset", @hideOrShowSuggestion, this

  render: ->
    $(@el).html(@template(singles: @collection))
    _.each(@recentSingles(), @prependSingle)
    this

  prependSingle: (single) =>
    view = new Cubemania.Views.Single(model: single)
    @$("ol").prepend(view.render().el)

  loadMore: (event) ->
    event.preventDefault()
    @recent = true
    @render()
    @hideOrShowSuggestion()
    @$(".load-more").hide()

  recentSingles: ->
    if @recent?
      @collection.recent(12)
    else
      @collection.today()

  hideOrShowSuggestion: (single) =>
    if @recentSingles().length == 0
      @$("p.suggestion").show()
    else
      @$("p.suggestion").hide()
