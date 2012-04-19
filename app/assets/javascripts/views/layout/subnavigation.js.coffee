class Cubemania.Views.Subnavigation extends Backbone.View
  template: JST["layout/subnavigation"]

  events:
    "click #puzzles a": "checkPuzzle"
    "click #kinds a": "checkKind"

  render: ->
    $(@el).html(@template(kinds: @collection))
    this

  checkPuzzle: (event) ->
    event.preventDefault()
    $("#puzzles ul.puzzles li").removeClass("checked")
    $(event.currentTarget).parent().addClass("checked")
    # TODO trigger event

  checkKind: (event) ->
    event.preventDefault()
    index = $(event.currentTarget).data("index")

    $("#kinds ul").children("li").removeClass("checked").eq(index).addClass("checked")
    $("#puzzles > ul").animate {left: -100 * index + "%"}, "normal"

  hide: ->
    $(@el).hide()

  show: ->
    $(@el).show()
