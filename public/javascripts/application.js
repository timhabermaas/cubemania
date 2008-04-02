function checkKind(index) {
  $("#kinds ul").children("li").removeClass("checked").eq(index).addClass("checked");
  $("#puzzles ul").animate({left: -100 * index + "%"}, "normal");
}

function showSubNavigation() {
  $("#subnavigation").slideDown("normal");
}

function embedChart(url) {
  swfobject.embedSWF('/movies/chart.swf', 'chart', '750', '400', '9.0.28.0', '/movies/expressInstall.swf', {dataUrl: url}, {wmode: 'transparent'});
}

function initTimer() {
  $(function() {
    $("#timer a").click(function() {
      toggleTimer();
    });
    $(window).keydown(function(event) {
      if (event.keyCode == 32) {
        toggleTimer();
      }
    });
  });
}

function toggleTimer() {
  if (this.interval == null) {
    $("#timer p").text(0);
    $("#timer a").text("Stop");
    this.interval = setInterval(function() {
      $("#timer p").text(Math.round((Number($("#timer p").text()) + 1/100) * 100) / 100);
    }, 10);
  }
  else {
    clearInterval(this.interval);
    this.interval = null;
    document.getElementById("chart").addItem(Number($("#timer p").text()) * 100, "UU FF RR");
    $("#timer a").text("GO!");
  }
}

function autocompleteUserName(url) {
  $(function() {
    $("#user_name").autocomplete({
      ajax: url,
      match: function(typed) { return this.name.match(new RegExp(typed)); },
      insertText: function(user) { return user.name }
    })
    .bind("activate.autocomplete", function(e, d) { document.getElementById("chart").addSeries(d.url) });
  });
}