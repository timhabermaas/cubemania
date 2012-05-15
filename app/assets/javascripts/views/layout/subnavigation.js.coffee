class Cubemania.Views.Subnavigation extends Cubemania.BaseView
  template: JST["layout/subnavigation"]

  events:
    "click #puzzles a": "puzzleClicked"
    "click #kinds a": "kindClicked"

  initialize: ->
    @intervalId = null

  render: ->
    $(@el).html(@template(kinds: @collection))
    this

  checkPuzzleAndKind: (puzzle) ->
    @$("#puzzles ul.puzzles li").removeClass("checked")
    @$("#puzzles ul.puzzles li").filter("[data-id=#{puzzle.get("id")}]").addClass("checked")

    kindIndex = @$("#kinds ul li").filter("[data-id=#{puzzle.get("kind_id")}]").data("index")
    @checkKind kindIndex

  puzzleClicked: (event) ->
    event.preventDefault()
    parent = $(event.currentTarget).parent()
    id = parent.data("id")
    Cubemania.currentPuzzle.set(Cubemania.puzzles.findByIdOrSlug(id))

    @resetTimerAndReshowPuzzles()

  kindClicked: (event) ->
    event.preventDefault()
    index = $(event.currentTarget).parent().data("index")

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
