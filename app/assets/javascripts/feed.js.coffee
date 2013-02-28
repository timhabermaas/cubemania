jQuery ->
  return unless $("#feed").length > 0

  $("a.comment-toggle").on "click", (event) ->
    anchor = $(this)
    if anchor.text() == "Show comments"
      anchor.parent().find("div.comments").slideDown()
    else
      anchor.parent().find("div.comments").slideUp()
    anchor.toggleText("Hide comments", "Show comments")
