$(document).ready ->
  ctx = $("#weeks-performance-graph").get(0).getContext('2d')
  submitData = []

  requestEventSummary()

  $(document).on 'event-summary-updated', (event) ->
    if event.eventSummary.name == "tab-submit"
      submitData = event.eventSummary.data

    labels = $.map(submitData, (elem) -> elem.name)
    submitDataset = $.map(submitData, (elem) -> elem.y)

    maxOfSubmits = Math.max.apply(null, submitDataset)
    minimumMaxValue = 50
    maxValue = Math.max(maxOfSubmits, minimumMaxValue)
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

    chartData = {
        labels: labels,
        datasets: [
            {
                data: submitDataset,
                fillColor: "rgba(0, 0, 0, 0)",
                strokeColor: "#1d93d6",
                pointColor: "#1d93d6"
            }
        ]
    }

    new Chart(ctx).Line(chartData, options)
