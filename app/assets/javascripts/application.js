// This is a manifest file that'll be compiled into including all the files listed below.
// Add new JavaScript/Coffee code in separate files in this directory and they'll automatically
// be included in the compiled file accessible from http://example.com/assets/application.js
// It's not advisable to add code directly here, but if you do, it'll appear at the bottom of the
// the compiled file.
//
//= require jquery
//= require jquery_ujs
//= require date
//= require highcharts
//= require jquery.tokeninput
//= require jquery.fancybox
//= require underscore
//= require backbone
//= require backbone.localStorage
//= require scramble_pyraminx
//= require scramble_2x2x2
//
//= require .//cubemania
//
//= require_tree ../templates/
//= require_tree .//models
//= require_tree .//collections
//= require_tree .//views
//= require_tree .//routers

//= require_tree .

document.cookie = 'tz_offset=' + ((new Date()).getTimezoneOffset() + calculateDst() * 6);

$(document).ready(function() {

  if ($("#timer").length > 0) {
    Cubemania.init();
  }
});
