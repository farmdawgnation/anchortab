stripeCallback = (status, response) ->
  if response.error
    anchortabSite.validationError("#card-number", response.error.message)
  else
    $("#stripe-token").val(response.id)
    $(".stripe-form").submit()

$(document).ready ->
  anchortabSite.event("stripe-form-ready")

  $(".stripe-form .submit").on "click", (event) ->
    return if $(".plan-selection").find(":selected").text().match(/\(Free\)$/)

    event.preventDefault()
    event.stopPropagation()

    anchortabSite.event("validate-stripe-form",
      stripeCallback: stripeCallback
    )
