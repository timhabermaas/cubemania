jQuery ->
  $("#timer .toggle").bind "click", (event) ->
    $("#timer #new_single").toggle()
    $("#timer .time").toggle()
    if $(this).text() == "Set times manually"
      $(this).text("Changed your mind?")
      $("#single_human_time").focus()
    else
      $(this).text("Set times manually")
    event.preventDefault()