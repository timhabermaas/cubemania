class Cubemania.Views.Subnavigation extends Cubemania.BaseView
  template: JST["layout/subnavigation"]

  events:
    "click #puzzles a": "puzzleClicked"
    "click #kinds a": "kindClicked"

  initialize: ->
    @bindTo Cubemania.currentPuzzle, "change", @checkPuzzleAndKind, this

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

  kindClicked: (event) ->
    event.preventDefault()
    index = $(event.currentTarget).parent().data("index")

    @checkKind index

  checkKind: (index) ->
    @$("#kinds ul").children("li").removeClass("checked").eq(index).addClass("checked")
    @$("#puzzles > ul").animate {left: -100 * index + "%"}, "normal"

  hide: ->
    $(@el).hide()

  show: ->
    $(@el).show()

###
TODO make subnavigation hideable
hideSubnavigation = () ->
  $("#subnavigation #puzzles").slideUp("slow")
  clearTimeout(intervalId)
  intervalId = null

showSubnavigation = () ->
  $("#subnavigation #puzzles").slideDown("slow")
  clearTimeout(intervalId)
  intervalId = setTimeout(hideSubnavigation, 7000)

jQuery ->
  if $("#timer").length
    showSubnavigation()

    $("#subnavigation #kinds a").bind "click", (event) ->
      showSubnavigation()
