var chart;

$(document).ready(function() {
  var singles = $("#singles li .time").map(function() {
    return parseInt($(this).attr("data-time"));
  }).get().reverse();

  chart = new Highcharts.Chart({
           chart: {
             renderTo: 'chart'
           },
           title: {
             text: 'Cubing Progress'
           },
           xAxis: {
           },
           yAxis: {
              title: {
                 text: 'Time'
              },
              labels: {
                formatter: function() {
                  return formatTime(this.value);
                }
              }
           },
           tooltip: {
             formatter: function() {
               return "hey :)";
             }
           },
           plotOptions: {
                    line: {
                       enableMouseTracking: true,
                       allowPointSelect: true
                    }
                 },
           series: [{
              name: $("#singles").attr("data-name"),
              data: singles
           }]
        });
});