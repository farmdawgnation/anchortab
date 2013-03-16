stripeCallback = (status, response) ->
  if response.error
    anchortabSite.validationError("#card-number", response.error.message)
  else
    $("#stripe-token").val(response.id)
    $(".stripe-form").submit()

$(document).ready ->
  anchortabSite.event("stripe-form-ready")

  $(".plan-selection").on 'change', (event) ->
    if $(".plan-selection").find(":selected").data("has-trial")
      $(".billing-information").hide()
    else
      $(".billing-information").show()

  $(".stripe-form .submit").on "click", (event) ->
    return if $(".plan-selection").find(":selected").data("has-trial")

    event.preventDefault()
    event.stopPropagation()

    anchortabSite.event("validate-stripe-form",
      stripeCallback: stripeCallback
    )

  $(".plan-selection").change()
