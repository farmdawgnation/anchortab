$(document).ready ->
  ctx = $("#weeks-performance-graph").get(0).getContext('2d')
  viewData = []
  submitData = []

  requestEventSummary()

  $(document).on 'event-summary-updated', (event) ->
    if event.eventSummary.name == "tab-view"
      viewData = event.eventSummary.data
    else if event.eventSummary.name == "tab-submit"
      submitData = event.eventSummary.data

    maxOfViews = Math.max.apply(null, viewData.y)
    maxOfSubmits = Math.max.apply(null, submitData.y)
    minimumMaxValue = 50
    maxValue = Math.max(maxOfViews, maxOfSubmits, minimumMaxValue)
    maxValueRemainder = maxValue % 10
    maxValueAdd = if maxValueRemainder > 0
      10 - maxValueRemainder
    else
      0
    ceilingValue = maxValue + maxValueAdd
    stepWidth = ceilingValue / 10

    options = {
      animation: false,
      scaleOverride: true,
      scaleSteps: 10,
      scaleStepWidth: stepWidth,
      scaleStartValue: 0
    }

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
