$(document).ready ->
  chart = new Highcharts.Chart
    chart:
      renderTo: 'weekly-performance-graph',
      defaultSeriesType: 'spline',
      backgroundColor: '#f7f7f7',
      width: 600
    title: [
      text: 'Weekly Performance'
    ],
    series: [
      {
        name: 'views',
        data: [['Mon', 10], ['Tue', 20], ['Wed', 30], ['Thu', 20], ['Fri', 10], ['Sat', 15], ['Sun', 10]]
      },
      {
        name: 'submits',
        data: [5, 1, 0, 0, 2, 3, 4]
      }
    ],
    legend:
      enabled: false
    yAxis:
      title:
        text: null
      min: 0

  requestEventSummary()

  $(document).on 'event-summary-updated', (event) ->
    if event.eventSummary.name == "tab-view"
      chart.series[0].setData(event.eventSummary.data)
    else if event.eventSummary.name == "tab-submit"
      chart.series[1].setData(event.eventSummary.data)
