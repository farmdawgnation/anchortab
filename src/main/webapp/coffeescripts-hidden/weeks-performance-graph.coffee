$(document).ready ->
  ctx = $("#weeks-performance-graph").get(0).getContext('2d')
  viewData = []
  submitData = []

  options = {
    scaleOverride: true,
    scaleSteps: 10,
    scaleStepWidth: 5,
    scaleStartValue: 0
  }

  requestEventSummary()

  $(document).on 'event-summary-updated', (event) ->
    if event.eventSummary.name == "tab-view"
      viewData = event.eventSummary.data
    else if event.eventSummary.name == "tab-submit"
      submitData = event.eventSummary.data

    labels = $.map(viewData, (elem) -> elem.name)
    viewDataset = $.map(viewData, (elem) -> elem.y)
    submitDataset = $.map(submitData, (elem) -> elem.y)

    chartData = {
        labels: labels,
        datasets: [
            {
                data: viewDataset,
                fillColor: "rgba(0, 0, 0, 0)",
                strokeColor: "#3f3f3e",
                pointColor: "#3f3f3e"
            },
            {
                data: submitDataset,
                fillColor: "rgba(0, 0, 0, 0)",
                strokeColor: "#1d93d6",
                pointColor: "#1d93d6"
            }
        ]
    }

    new Chart(ctx).Line(chartData, options)
