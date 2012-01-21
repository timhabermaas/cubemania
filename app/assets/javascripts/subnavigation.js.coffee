root = exports ? this

root.checkKind = (index) ->
  $("#kinds ul").children("li").removeClass("checked").eq(index).addClass("checked")
  $("#puzzles > ul").animate {left: -100 * index + "%"}, "normal"

hideSubnavigation = () ->
  $("#subnavigation #puzzles").slideUp("slow")

showSubnavigation = () ->
  $("#subnavigation #puzzles").slideDown("slow")

jQuery ->
  if $("#timer").length
    $("#subnavigation #puzzles").hide();
    $("#subnavigation").hoverIntent(
      over: showSubnavigation
      timeout: 1000
      out: hideSubnavigation
    )
