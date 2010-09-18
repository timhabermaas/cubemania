var chart;

$(document).ready(function() {
  var singles = $("#singles li time").map(function() {
    return $(this).attr("data-time");
  }).get();

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
           plotOptions: {
                    line: {
                       dataLabels: {
                          enabled: true
                       },
                       enableMouseTracking: false
                    }
                 },
           series: [{
              name: 'tim',
              data: singles
           }]
        });
});