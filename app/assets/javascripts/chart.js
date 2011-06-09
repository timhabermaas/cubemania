var chart;

$(document).ready(function() {
  var singles = $("#singles li #time").map(function() {
    return parseInt($(this).data("time"));
  }).get().reverse();

  chart = new Highcharts.Chart({
           chart: {
             renderTo: 'chart'
           },
           title: {
             text: 'Cubing Progress'
           },
           xAxis: {
             title: {
               text: 'Attempts'
             },
             allowDecimals: false,
             labels: {
               formatter: function() {
                 return this.value + 1;
               }
             }
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