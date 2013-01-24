jQuery(function() {
  if (!$("#activity").length) {
    return;
  }

  var week = d3.time.format("%W"),
      day  = d3.time.format("%w");

  var monthNames = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];

  function monthName(d) {
    return monthNames[d.getMonth()];
  }

  function parseDate(string) {
    var parts = string.match(/(\d+)/g);
    return new Date(parts[0], parseInt(parts[1]) - 1, parts[2]);
  }

  var cellSize = 12;
  var strokeWidth = 6;
  var gap = 1;
  var data = $("#activity").data("activity");
  var svg = d3.select("#activity")
    .append("svg")
    .attr("width", "100%")
    .attr("height", cellSize * 7 + 20 + 5)
    .attr("font-size", 14);

  // page is 960px wide; svg element is displayed as 656px, so move it (960-656)/2 to the right
  // well, almost...
  var container = svg
    .append("g")
    .attr("transform", "translate(190, 20)");

  data = d3.map(data);

  var newData = d3.map();
  data.forEach(function(k, v) {
    newData.set(parseDate(k), v);
  });

  var today = new Date();
  var oneYearAgo = d3.time.year.offset(today, -1);

  var weeks = container.selectAll("g.week")
    .data(d3.time.mondays(oneYearAgo, d3.time.monday.ceil(today)))
    .enter()
    .append("g")
    .attr("class", "week")
    .attr("transform", function(d, i) {
      return "translate(" + i * cellSize + ",0)";
    });

  function hasBeginningOfMonthInWeek(week) {
    var nextWeek = d3.time.week.offset(week, 1);
    return week.getMonth() !== nextWeek.getMonth();
  }

  weeks.append("text")
    .text(function(d, i) {
      if (hasBeginningOfMonthInWeek(d)) {
        return monthName(d3.time.day.offset(d, 7));
      } else {
        return "";
      }
    })
    .attr("y", -5)
    .attr("fill", "#b5cadf");

  container
    .append("g")
    .attr("class", "weekdays")
    .selectAll("text")
    .data(["Mon", "Wed", "Fri", "Sun"])
    .enter()
    .append("text")
    .text(function(d) {
      return d;
    })
    .attr("fill", "#b5cadf")
    .attr("x", -35)
    .attr("y", function(d, i) {
      return cellSize * 2 * i + cellSize - 2 * gap;
    });

  var color = d3.scale.linear()
    .domain([0, 1, 50, 100, 200, 400])
    .range(["#8BA4C0", "#C7E9C0", "#A1D99B", "#74C476", "#31A354", "#006D2C"])
    .clamp(true);

  function countFor(date) {
    var c = newData.get(date);
    return c ? c : 0;
  }

  var days = weeks.selectAll("rect")
    .data(function(d) {
      var nextMonday = d3.time.week.offset(d, 1);
      if (today < nextMonday) {
        nextMonday = today;
      }
      return d3.time.days(d, nextMonday);
     })
    .enter()
    .append("rect")
    .attr("y", function(d, i) {
      return i * cellSize;
    })
    .attr("width", cellSize - 2 * gap)
    .attr("height", cellSize - 2 * gap)
    .attr("fill", function(d, i) {
      return color(countFor(d));
    });

  var tooltip = container.append("g")
    .attr("opacity", 0)
    .attr("transform", "translate(-200, -200)");

  tooltip.append("rect")
    .attr("width", cellSize + strokeWidth)
    .attr("height", cellSize + strokeWidth)
    .attr("x", -strokeWidth / 2 - gap)
    .attr("y", -strokeWidth / 2 - gap)
    .attr("fill", "none")
    .attr("stroke", "black")
    .attr("stroke-width", strokeWidth);

  var tempX = cellSize + strokeWidth - gap;
  var tempY = -strokeWidth - gap;

  var box = tooltip.append("g")
    .attr("transform", "translate(" + tempX + "," + tempY + ")");

  box.append("rect")
    .attr("height", cellSize + 2 * strokeWidth)
    .attr("class", "box");

  box.append("text")
    .attr("fill", "#df0")
    .attr("y", strokeWidth + cellSize / 2)
    .attr("dy", ".35em");

  function pluralize(singular, count) {
    if (count == 1) {
      return count + " " + singular;
    } else {
      return count + " " + singular + "s";
    }
  }

  days
    .on("mouseover", function(d, i) {
      if (newData.get(d)) {
        rect = d3.select(this);
        g = d3.select(this.parentElement);
        var x = g[0][0].transform.baseVal.getItem(0).matrix.e;
        var y = rect[0][0].y.baseVal.value;
        var solveCount = countFor(d);
        var boxWidth = 65 + (solveCount.toString().length - 1) * 9;
        tooltip
          .attr("transform", "translate(" + x + "," + y + ")")
          .attr("opacity", 1)
          .select("text")
          .text(pluralize("Solve", solveCount));
        tooltip.select("rect.box")
          .attr("width", boxWidth);
      }
    })
    .on("mouseout", function() {
      tooltip
        .attr("opacity", 0)
        .attr("transform", "translate(-100, -100)");

    })
    .append("title")
    .text(function(d) {
      return formatDate(d);
    });
});
