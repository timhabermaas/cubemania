/*var timerStarted = false;
var startTime;
var intervalId = null;
var justStopped = false;
var timerEnabled = true;

$(function() {
  $("textarea, input[type=text]").bind('focus', function() {
    timerEnabled = false;
  });
  $("textarea, input[type=text]").bind('blur', function() {
    timerEnabled = true;
  });

  $(document).keyup(function(event) {
    if (event.keyCode == 32 && !timerStarted && timerEnabled) {
      if (justStopped) {
        justStopped = false;
      } else {
        $("#timer .time").removeClass("starting");
        startTimer();
      }
      event.preventDefault();
    }
  });

  $(document).keydown(function(event) {
    if (event.keyCode == 32 && timerEnabled) {
      if (timerStarted) {
        stopTimer();
        justStopped = true;
      } else {
        $("#timer .time").addClass("starting");
      }
      event.preventDefault();
    }
  });

  updateStatistics();
});

function startTimer() {
  timerStarted = true;
  startTime = new Date().getTime();
  intervalId = setInterval(updateDisplay, 42)
}

function updateDisplay() {
  $("#timer .time").text(formatTime(new Date().getTime() - startTime));
}

function stopTimer() {
  updateDisplay();
  $("#timer #single_time").val(new Date().getTime() - startTime);
  $("#timer #single_human_time").val("");
  $("#timer form").submit();
  clearInterval(intervalId);
  timerStarted = false;
}

function getSingles() {
  return $("#singles li #time");
}

function average(singles, size) {
  var singles = singles.slice(0, size);
  var dnfs = singles.filter(".dnf");

  var sum = 0;

  if (dnfs.length > 1 || singles.length < size) {
    return null;
  }
  if (dnfs.length == 1) {
    singles = singles.not(".dnf");
    var times = singles.map(function() {
      return parseInt($(this).attr("data-time"));
    }).get();
    return (Array.sum(times) - Array.min(times)) / (times.length - 1);
  } else {
    var times = singles.map(function() {
      return parseInt($(this).attr("data-time"))
    }).get();
    return (Array.sum(times) - Array.min(times) - Array.max(times)) / (times.length - 2);
  }
}

function updateStatistics() {
  var singles = getSingles();
  var cur5 = average(singles, 5);
  var cur12 = average(singles, 12);
  if (cur5 == null) {
    $("#cur5").replaceWith("-");
  } else {
    $("#cur5").replaceWith(formatTime(cur5));
  }
  if (cur12 == null) {
    $("#cur12").replaceWith("-");
  } else {
    $("#cur12").replaceWith(formatTime(cur12));
  }
}*/