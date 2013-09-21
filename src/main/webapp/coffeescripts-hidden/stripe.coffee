$(document).ready ->
  Stripe?.setPublishableKey? 'pk_live_fJ4ubfL69lbYqhJWX8IFk95V'

  $("body").on 'click', '.stripe-form .submit, .stripe-button', (event) ->
    $target = $(event.target)

    if $target.is '.stripe-button'
      $target.hide()
    else
      $target.addClass("disabled")

    $target.after(
      $("<img />")
        .addClass("stripe-ajax-loader")
        .attr("src", "/images/ajax-loader.gif"))

    setTimeout ->
      $target.attr("disabled", "disabled")

      if $target.is '.stripe-button'
        $(".stripe-button").attr("disabled", "disabled").addClass("disabled")

  $(document).on 'form-validation-error, general-error', (event) ->
    setTimeout ->
      $(".stripe-ajax-loader").remove()
      $(".submit").removeClass("disabled").removeAttr("disabled")

$(document).on 'stripe-form-ready', (event) ->
  $('#card-number').payment('formatCardNumber')
  $('#card-expiry').payment('formatCardExpiry')
  $("#card-cvc").payment('formatCardCVC')

$(document).on "validate-stripe-form", (event) ->
  $(".validation-error").remove()
  $("input.error").removeClass("error")

  validationError = false

  unless $.payment.validateCardNumber($("#card-number").val())
    anchortabSite.validationError("#card-number", "Invalid card number.")
    validationError = true

  unless $.payment.validateCardCVC($("#card-cvc").val())
    anchortabSite.validationError("#card-cvc", "Invalid CVC.")
    validationError = true

  cardExpiry = $("#card-expiry").payment('cardExpiryVal')

  unless $.payment.validateCardExpiry(cardExpiry.month, cardExpiry.year)
    anchortabSite.validationError("#card-expiry", "Invalid card expiration.")
    validationError = true

  unless validationError
    Stripe.createToken
      number: $('#card-number').val(),
      cvc: $('#card-cvc').val(),
      exp_month: cardExpiry.month,
      exp_year: cardExpiry.year,
      address_zip: $("#card-billing-zip").val()
    , event.stripeCallback
