/*function setPuzzlesWidth(id) {
  var ul = $("#puzzles_" + id + " ul");
  var li = ul.children("li");
  var width = li.width() + parseFloat(li.css('margin-right'));
  ul.width(++li.length * width);
}*/

function showPuzzles(index) {
  $("#kinds").children("li").removeClass("checked").eq(index).addClass("checked");
  var value = -$("#puzzles li").height() * index;
  $("#puzzles ul").animate({top: value}, "normal");
}