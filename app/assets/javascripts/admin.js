// Generated by CoffeeScript 1.10.0
(function() {
  jQuery(function() {
    return $("section.admin a.toggle").bind("click", function(event) {
      $(this).toggleText($(this).data("text"), "Changed your mind?");
      $("div.form").slideToggle("fast");
      return event.preventDefault();
    });
  });

}).call(this);
