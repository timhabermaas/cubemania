var startColor = 7;

var maxDots = 0;

var plot;

var options = {
  series: {
    lines: {
      show: true,
      lineWidth: 4
    },
    points: {
      show: true,
      radius: 5
    }
  },
  grid: {
    hoverable: true,
    clickable: true,
    color: "#fff",
    tickColor: "#6c89a9"
  },
  xaxisIndex: {
    mode: null,
    ticks: 5,
    tickDecimals: 0
  },
  xaxisDate: {
    mode: "time",
    ticks: 5,
    timeformat: "%b, %d %y"
  },
  xaxis: {
    mode: null,
    ticks: 5,
    tickDecimals: 0
  },
  yaxis: {
    tickFormatter: function(t, axis) {
      if (t >= 60) {
        min = Math.floor(t / 60);
        sec = t - min * 60;
        sec = sec < 10 ? "0" + sec.toFixed(0) : sec.toFixed(0);
        return min + ":" + sec + " min";
      } else {
        return t.toFixed(2) + " s";
      }
    },
    labelWidth: 70,
  },
  legend: {
    labelFormatter: function(label, series) {
      return '<a href="#" class="legend">' + label + '</a>';
    },
    backgroundColor: null,
    backgroundOpacity: 0,
    margin: [5, 10]
  }
};


function switchToDateView() {
  options.xaxis = options.xaxisDate;
  averages = $("#times #chart").data("averages");
  for (i = 0; i < averages.length; i++) {
    for (j = 0; j < averages[i].data.length; j++) {
      averages[i].data[j][0] = averages[i].dates[j];
    }
  }
  $.plot($("#chart"), averages, options);
}

function switchToIndexView() {
  options.xaxis = options.xaxisIndex;
  averages = $("#times #chart").data("averages");
  for (i = 0; i < averages.length; i++) {
    for (j = 0; j < averages[i].data.length; j++) {
      averages[i].data[j][0] = j;
    }
  }
  $.plot($("#chart"), averages, options);
}

function addItem(data) {
  addItemWithoutPlot(data);
  $.plot($('#chart'), $("#times #chart").data("averages"), options);
}

function addItemWithoutPlot(data, index) {
  if (index == null)
    index = 0;
  var averages = $("#times #chart").data("averages");
  if (options.xaxis.mode == null) {
    averages[index].data.push([averages[index].data.length, data.time]);
  } else {
    averages[index].data.push([data.created_at, data.time]);
  }
  averages[index].urls.push(data.url);
  averages[index].tooltips.push(data.tooltip);
  averages[index].dates.push(data.created_at);
}

function showAjaxLoader() {
  $("#times #chart").css("background", "url(/images/ajax-loader.gif) 362px 50% no-repeat");
}
function hideAjaxLoader() {
  $("#times #chart").css("background", "none");
}

function showTooltip(x, y, content, right) {
  $("#tooltip").html(content);
  if (right) {
    offset = 10;
  } else {
    offset = -$("#tooltip").width() - 10;
  }
  $("#tooltip").css({
    top: y + 10,
    left: x + offset
  });
  $("#tooltip").fadeIn(100);
}
function hideTooltip() {
  $("#tooltip").hide();
}

function updateMaxDots() {
  maxDots = 0;
  $.each($("#times #chart").data("averages"), function(i, item) {
    if (item.data.length > maxDots) {
      maxDots = item.data.length;
    }
  });
}

function addSeries(url) {
  showAjaxLoader();
  $.getJSON(url, function(data) {
    var averages = $("#times #chart").data("averages");
    var new_averages = {
      color: startColor + averages.length,
      label: data.name,
      tooltips: [],
      data: [],
      dates: [],
      urls: []
    };
    var index = averages.push(new_averages);
    $.each(data.averages.reverse(), function(i, item) {
      addItemWithoutPlot(item, index - 1);
    });
    plot = $.plot($('#chart'), averages, options);
    hideAjaxLoader();
    updateMaxDots();
  });
}

function removeSeries(name) {
  var averages = $("#times #chart").data("averages")
  $.each(averages, function(i, item) {
    if (item.label == name && i != 0) {
      averages.splice(i, 1);
      updateMaxDots();
      plot = $.plot($('#chart'), averages, options);
    }
  });
}

$("#times #chart").live("plothover", function(event, pos, item) {
  if (item) {
    var min = item.series.xaxis.min;
    var max = item.series.xaxis.max;
    if ((pos.x - min)/(max - min) >= 0.5) {
      showTooltip(item.pageX, item.pageY, item.series.tooltips[item.dataIndex], false);
    } else {
      showTooltip(item.pageX, item.pageY, item.series.tooltips[item.dataIndex], true);
    }
  } else {
    hideTooltip();
  }
});

$(document).ready(function() {

  if ($("#times #chart").length == 0)
    return;

  $("#times #chart").bind("plotclick", function(event, pos, item) {
    if (item) {
      plot.unhighlight();
      plot.highlight(item.series, item.datapoint);
      $.getScript(item.series.urls[item.dataIndex]);
    }
  });

  $("#times #user").change(function() {
    url = $("#times #user option:selected:first").val();
    // keep list of added names to compare and don't add the same user twice
    addSeries(url)
  });

  $("#times #chart").data("averages", []);

  var url = $("#times #chart").attr("data-url");

  addSeries(url);

  $("#times #formats a.mode").toggle(function() {
    $(this).html('<a href="#">Display by Index</a>');
    switchToDateView();
  }, function() {
    $(this).html('<a href="#">Display by Date</a>');
    switchToIndexView();
  });

  $("a.legend").live("click", function() {
    removeSeries($(this).html());
    return false;
  });

});