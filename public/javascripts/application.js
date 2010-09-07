function checkKind(index) {
  $("#kinds ul").children("li").removeClass("checked").eq(index).addClass("checked");
  $("#puzzles > ul").animate({left: -100 * index + "%"}, "normal");
}

function tooltip() {
  xOffset = 10;
  yOffset = 20;
  $(".tooltip").hover(function(e) {
    if (this.title == "") return;
    this.t = this.title;
    this.title = "";
    $("#tooltip").text(this.t);
    $("#tooltip")
      .animate({top: (e.pageY - xOffset) + "px", left: (e.pageX + yOffset) + "px"}, {queue: false, duration: 400})
      .fadeIn("normal");
    },
  function() {
    if (this.t == null) return;
    this.title = this.t;
    $("#tooltip").hide();
  });
};

function formatTime(time) {
  var seconds = (time / 1000).toFixed(2);
  if (seconds < 60) {
    return seconds;
  } else {
    var minutes = Math.floor(seconds / 60);
    seconds = seconds - minutes * 60;
    var s = seconds < 10 ? "0" : "";
    return minutes + ":" + s + seconds.toFixed(2);
  }
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

$(document).ready(function() {
  tooltip();
  $("#users_search input[type=text]").liveUpdate("#users");
  $("#users_search input[type=text]").Watermark("Type a name");
});