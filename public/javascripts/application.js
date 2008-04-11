function checkKind(index) {
  $("#kinds ul").children("li").removeClass("checked").eq(index).addClass("checked");
  $("#puzzles > ul").animate({left: -100 * index + "%"}, "normal");
}