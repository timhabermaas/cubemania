var timerStarted = false;
var startTime;
var intervalId = null;
var justStopped = false;
var timerEnabled = true;

$(document).ready(function() {
  $('#timer #toggle').bind('click', function() {
    $('#single_human_time').toggle();
    $("#timer time").toggle();
    $('#timer form input[type=submit]').toggle();
    $(this).text($(this).text() == 'Set times manually' ? 'Changed your mind?' : 'Set times manually');
    return false;
  });

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
        $("#timer time").removeClass("starting");
        startTimer();
      }
      return false;
    }
  });

  $(document).keydown(function(event) {
    if (event.keyCode == 32 && timerEnabled) {
      if (timerStarted) {
        stopTimer();
        justStopped = true;
      } else {
        $("#timer time").addClass("starting");
      }
      return false;
    }
  });
});

function startTimer() {
  timerStarted = true;
  startTime = new Date().getTime();
  intervalId = setInterval(updateDisplay, 42)
}

function updateDisplay() {
  $("#timer time").text(formatTime(new Date().getTime() - startTime));
}

function stopTimer() {
  updateDisplay();
  $("#timer #single_time").val(new Date().getTime() - startTime);
  $("#timer #single_human_time").val("");
  $("#timer form").submit();
  clearInterval(intervalId);
  timerStarted = false;
}

function updateStatistics() {
  // oh yeah :)
}