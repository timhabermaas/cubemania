function checkKind(index) {
  $("#kinds ul").children("li").removeClass("checked").eq(index).addClass("checked");
  $("#puzzles > ul").animate({left: -100 * index + "%"}, "normal");
}

$.fn.textToggle = function(open, close) {
  $(this).text($(this).text() == open ? close : open);
};

function calculateDst() {
  var rightNow = new Date();
  var jan1 = new Date(rightNow.getFullYear(), 0, 1, 0, 0, 0, 0);
  var june1 = new Date(rightNow.getFullYear(), 6, 1, 0, 0, 0, 0);
  var temp = jan1.toGMTString();
  var jan2 = new Date(temp.substring(0, temp.lastIndexOf(" ") - 1));
  temp = june1.toGMTString();
  var june2 = new Date(temp.substring(0, temp.lastIndexOf(" ") - 1));
  var std_time_offset = (jan1 - jan2) / (1000 * 60 * 60);
  var daylight_time_offset = (june1 - june2) / (1000 * 60 * 60);
  if (std_time_offset == daylight_time_offset) {
    return 0;
  } else {
    return 1;
  }
}

document.cookie = 'tz_offset=' + ((new Date()).getTimezoneOffset() + calculateDst() * 60);