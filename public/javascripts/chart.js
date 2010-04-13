var averages = [];

var options = {
  series: {
    lines: { 
      show: true,
      lineWidth: 3
    },
    points: {
      show: true,
      radius: 4
    }
  },
  grid: {
    hoverable: true,
    color: "#fff",
    tickColor: "#6c89a9"
  },
  xaxis: {
    mode: "time",
    timeFormat: "%B %d, %Y at %H:%M",
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

function addItem(data) {
  averages[0].data.push([data.created_at, data.time]);
  averages[0].tooltips.push(data.tooltip);
  $.plot($('#chart'), averages, options);
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

$("#times #chart").ready(function() {

  var startColor = 7;
  var url = $("#times #chart").attr("data-url");
  
  $("#times #chart").css("background", "url(/images/ajax-loader.gif) center center no-repeat");

  $.getJSON(url, function(data) {
    var raw_data = data.averages;
    var result = [];
    var tooltips = [];
    $.each(raw_data, function(i, item) {
      result.push([item.created_at, item.time]);
      tooltips.push(item.tooltip);
    });
    averages = [
    {
      color: startColor,
      label: data.name,
      tooltips: tooltips,
      data: result
    }];
    $("#times #chart").css("background", "none");
    plot = $.plot($('#chart'), averages, options);
  });
});