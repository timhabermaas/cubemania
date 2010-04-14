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
      return '<a href="#' + label + '">' + label + '</a>';
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

function addItemWithoutPlot(data) {
  var averages = $("#times #chart").data("averages");
  if (options.xaxis.mode == null) {
    averages[0].data.push([averages[0].data.length, data.time]);
  } else {
    averages[0].data.push([data.created_at, data.time]);
  }
  averages[0].tooltips.push(data.tooltip);
  averages[0].dates.push(data.created_at);
}

function showTooltip(x, y, content) {
  $("#tooltip").html(content);
  $("#tooltip").css({
    top: y + 5,
    left: x + 5
  });
  $("#tooltip").fadeIn(100);
}

function hideTooltip() {
  $("#tooltip").hide();
}

$("#times #chart").live("plothover", function(event, pos, item) {
  if (item) {
      showTooltip(item.pageX, item.pageY, item.series.tooltips[item.dataIndex]);
  } else {
    hideTooltip();
  }
});

$(document).ready(function() {

  if ($("#times #chart").length == 0)
    return;

  var startColor = 7;
  var url = $("#times #chart").attr("data-url");
  
  $("#times #chart").css("background", "url(/images/ajax-loader.gif) center center no-repeat");

  $.getJSON(url, function(data) {
    var averages = [
    {
      color: startColor,
      label: data.name,
      tooltips: [],
      data: [],
      dates: []
    }];
    $("#times #chart").data("averages", averages);
    options.xaxis = options.xaxisIndex;
    $.each(data.averages.reverse(), function(i, item) {
      addItemWithoutPlot(item);
    });
    $("#times #chart").css("background", "none");
    $("#times #chart").data("plot", $.plot($('#chart'), averages, options));
  });

  $('#times #formats a').toggle(function() {
    $(this).html('<a href="#">Display by Index</a>');
    switchToDateView();
  }, function() {
    $(this).html('<a href="#">Display by Date</a>');
    switchToIndexView();
  });
});

