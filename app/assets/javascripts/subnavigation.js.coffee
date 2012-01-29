root = exports ? this

root.checkKind = (index) ->
  $("#kinds ul").children("li").removeClass("checked").eq(index).addClass("checked")
  $("#puzzles > ul").animate {left: -100 * index + "%"}, "normal"

intervalId = null

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

