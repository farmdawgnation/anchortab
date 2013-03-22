$(document).ready ->
  Stripe?.setPublishableKey? 'pk_test_UQLI1joAA4aIZcXjESwa4awL'

  $("body").on 'click', '.stripe-form .submit', (event) ->
    $(event.target).addClass("disabled").after(
      $("<img />")
        .addClass("stripe-ajax-loader")
        .attr("src", "/images/ajax-loader.gif"))

    setTimeout ->
      $(event.target).attr("disabled", "disabled")

  $(document).on 'form-validation-error', (event) ->
    setTimeout ->
      $(".stripe-ajax-loader").remove()
      $(".submit").removeClass("disabled").removeAttr("disabled")

$(document).on 'stripe-form-ready', (event) ->
  $('#card-number').payment('formatCardNumber')
  $('#card-expiry').payment('formatCardExpiry')
  $("#card-cvc").payment('formatCardCVC')
  $("#card-billing-zip").payment('restrictNumeric')

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

  unless $("#card-billing-zip").val().length == 5
    anchortabSite.validationError("#card-billing-zip", "ZIP codes are five digits.")
    validationError = true

  unless validationError
    Stripe.createToken
      number: $('#card-number').val(),
      cvc: $('#card-cvc').val(),
      exp_month: cardExpiry.month,
      exp_year: cardExpiry.year,
      address_zip: $("#card-billing-zip").val()
    , event.stripeCallback
