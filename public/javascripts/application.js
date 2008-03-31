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