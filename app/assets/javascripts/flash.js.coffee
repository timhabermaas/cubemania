jQuery ->
  $("div.flash a.close").bind "click", (event) ->
    $("div.flash").slideUp("fast");

  $(document).ajaxComplete (e, request, options) ->
    message = request.getResponseHeader("X-Message")
    type = request.getResponseHeader("X-Type")

    if message
      $("div.flash.#{type} p").html(message);
      $("div.flash.#{type}").show();
