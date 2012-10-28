jQuery ->
  if $("#user").length
    $("td.avg12").hide()

    $("a.avg5, a.avg12").on "click", (event) ->
      event.preventDefault()

      $("td.avg5").toggle()
      $("td.avg12").toggle()
      $("a.avg5").toggleClass("selected")
      $("a.avg12").toggleClass("selected")
