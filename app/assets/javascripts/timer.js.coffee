hideSubnavigation = () ->
  $("#subnavigation #puzzles").slideUp("slow")

showSubnavigation = () ->
  $("#subnavigation #puzzles").slideDown("slow")

jQuery ->
  $("#timer #toggle").bind "click", (event) ->
    $("#timer #new_single").toggle()
    $("#timer .time").toggle()
    if $(this).text == "Set times manually"
      $(this).text("Changed your mind?")
      $("#single_human_time").focus()
    else
      $(this).text("Set times manually")
    event.preventDefault()

  if $("#timer").length
    $("#subnavigation #puzzles").delay(5000).slideUp("slow");
    $("#subnavigation").hoverIntent(
      over: showSubnavigation
      timeout: 1000
      out: hideSubnavigation
    )
