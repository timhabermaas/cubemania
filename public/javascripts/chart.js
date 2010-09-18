$(document).ready(function() {
  var singles = $("#timer #singles li time").map(function() {
    return $(this).attr("data-time");
  }).get();
});