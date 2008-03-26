function checkKind(index) {
  $("#kinds ul").children("li").removeClass("checked").eq(index).addClass("checked");
  $("#puzzles ul").animate({left: -1000 * index}, "normal");
}

function showSubNavigation() {
  $("#subnavigation").slideDown("normal");
}