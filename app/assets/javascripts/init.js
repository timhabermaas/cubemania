document.cookie = 'tz_offset=' + (new Date()).getTimezoneOffset();

$(document).ready(function() {
  if ($("#subnavigation").length > 0) {
    Cubemania.subnavigationView = new Cubemania.Views.Subnavigation({el: $("#subnavigation")});
  }
  if ($("#backbone-container").length > 0) {
    Cubemania.init();
  }
});
