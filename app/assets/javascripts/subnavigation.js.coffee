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
