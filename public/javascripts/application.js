function checkKind(index) {
  $("#kinds ul").children("li").removeClass("checked").eq(index).addClass("checked");
  $("#puzzles > ul").animate({left: -100 * index + "%"}, "normal");
}

$.fn.textToggle = function(open, close) {
  $(this).text($(this).text() == open ? close : open);
};

document.cookie = 'tz_offset=' + (new Date()).getTimezoneOffset();