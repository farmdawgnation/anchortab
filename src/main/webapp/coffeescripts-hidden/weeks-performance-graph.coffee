$(document).ready ->
  chart = new Highcharts.Chart
    chart:
      renderTo: 'weeks-performance-graph',
      defaultSeriesType: 'spline',
      backgroundColor: '#f7f7f7',
      width: 700,
      animation: false
    title: [
      text: 'Weekly Performance'
    ],
    series: [
      {
        name: 'views',
      },
      {
        name: 'submits',
      }
    ],
    legend:
      enabled: false
    xAxis:
      labels:
        enabled: false
    yAxis:
      title:
        text: null
      min: 0
      minRange: 10

  requestEventSummary()

  $(document).on 'event-summary-updated', (event) ->
    if event.eventSummary.name == "tab-view"
      chart.series[0].setData(event.eventSummary.data)
    else if event.eventSummary.name == "tab-submit"
      chart.series[1].setData(event.eventSummary.data)
