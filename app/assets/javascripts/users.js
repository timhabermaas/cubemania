jQuery(function() {
  if (!$("#users")) {
    return;
  }

  $(".users-container").on("click", ".pagination a", function(event) {
    var href = $(this).attr("href");

    $.get(href, function(data) {
      var $page = $(data);
      $("#users").append($(data).find("#users").html());
      $(".pagination").replaceWith($page.find(".pagination"));
    });
    event.preventDefault();
  })
});
