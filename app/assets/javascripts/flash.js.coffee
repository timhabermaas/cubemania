jQuery ->
  $("div.flash a.close").bind "click", (event) ->
    $("div.flash").slideUp("fast");

  $(document).ajaxComplete (e, request, options) ->
    message = request.getResponseHeader("X-Message")

    if message
      $("div.flash p").html(message);
      $("div.flash").show();
