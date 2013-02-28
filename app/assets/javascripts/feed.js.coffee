jQuery ->
  return unless $("#feed").length > 0

  $("a.comment-toggle").on "click", (event) ->
    anchor = $(this)
    comments = anchor.parent().parent().find("div.comments")
    if comments.is(":visible")
      comments.slideUp()
      anchor.text(anchor.data("show-text"))
    else
      comments.slideDown()
      anchor.text(anchor.data("hide-text"))
