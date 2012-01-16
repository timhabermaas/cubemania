hideSubnavigation = () ->
  $("#subnavigation #puzzles").slideUp("slow")

showSubnavigation = () ->
  $("#subnavigation #puzzles").slideDown("slow")

jQuery ->
  $("#subnavigation #puzzles").slideUp(400);
  $("#subnavigation").hoverIntent(
    over: showSubnavigation
    timeout: 1000
    out: hideSubnavigation
  )
