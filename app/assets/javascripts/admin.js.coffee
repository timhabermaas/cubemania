jQuery ->
  $("section.admin a.toggle").bind "click", (event) ->
    $(this).textToggle($(this).data("text"), "Changed your mind?")
    $("div.form").slideToggle("fast")
    event.preventDefault();