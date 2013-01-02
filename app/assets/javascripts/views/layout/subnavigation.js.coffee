class Cubemania.Views.Subnavigation extends Cubemania.BaseView
  events:
    "click #kinds a": "kindClicked"

  initialize: ->
    @intervalId = null

  kindClicked: (event) ->
    event.preventDefault()
    index = $(event.currentTarget).parent().index()
    @checkKind index

    @resetTimerAndReshowPuzzles()

  checkKind: (index) ->
    @$("#kinds ul").children("li").removeClass("checked").eq(index).addClass("checked")
    @$("#puzzles > ul").animate {left: -100 * index + "%"}, "normal"

  makeAutoHideable: ->
    clearTimeout(@intervalId)
    @intervalId = setTimeout(@slidePuzzlesUp, 7000)

  unmakeAutoHideable: ->
    clearTimeout(@intervalId)
    @intervalId = null

  resetTimerAndReshowPuzzles: ->
    clearTimeout(@intervalId)
    @intervalId = setTimeout(@slidePuzzlesUp, 7000) if @intervalId?
    @slidePuzzlesDown()

  hide: ->
    $(@el).hide()

  show: ->
    $(@el).show()
    @$("#puzzles").show()

  slidePuzzlesUp: ->
    @$("#puzzles").slideUp("slow")

  slidePuzzlesDown: ->
    @$("#puzzles").slideDown("slow")
