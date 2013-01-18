class Cubemania.Views.Singles extends Backbone.View
  template: JST["timer/singles"]

  events:
    "click .load-more a": "loadMore"

  initialize: ->
    @collection.on "reset", @onReset, this
    @collection.on "add", @prependSingle, this

    @collection.on "add", @increaseIndex, this
    @collection.on "remove", @decreaseIndex, this

    @collection.on "reset", @hideOrShowSuggestion, this
    @collection.on "add", @hideOrShowSuggestion, this
    @collection.on "remove", @hideOrShowSuggestion, this

    @nextSingleIndex = 0

  onReset: ->
    @nextSingleIndex = @collection.today().length
    @render()

  render: ->
    $(@el).html(@template(singles: @collection))
    _.each(@recentSingles(), @prependSingle)
    this

  prependSingle: (single) =>
    view = new Cubemania.Views.Single(model: single)
    @$("ol").prepend(view.render().el)

  appendSingle: (single) =>
    view = new Cubemania.Views.Single(model: single)
    @$("ol").append(view.render().el)

  loadMore: (event) ->
    event.preventDefault()
    singles = @collection.models[-(@nextSingleIndex + 12)..(-@nextSingleIndex - 1)]
    _.each(singles.reverse(), @appendSingle)
    @nextSingleIndex += 12
    @hideOrShowSuggestion()

  increaseIndex: ->
    @nextSingleIndex += 1

  decreaseIndex: ->
    @nextSingleIndex -= 1

  recentSingles: ->
    if @nextSingleIndex == 0
      []
    else
      @collection.models[-@nextSingleIndex..-1]

  hideOrShowSuggestion: ->
    if @nextSingleIndex == 0
      @$("p.suggestion").show()
    else
      @$("p.suggestion").hide()
