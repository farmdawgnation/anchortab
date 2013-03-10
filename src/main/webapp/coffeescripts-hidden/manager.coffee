modal = (id, nodes, closeCallback) ->
  closeCallback = closeCallback || (event) ->
    $(event.target).remove()

  $("#" + id).remove()

  modal =
    $("<div />")
      .addClass("modal")
      .attr("id", id)

  $.each(nodes, (i, node) ->
    modal.append(node)
  )

  modal.on $.modal.CLOSE, closeCallback

  $("body").append(modal)
  modal.modal()

$(document).ready ->
  $("select").customSelect()

  $(document).on 'login-failed', ->
    $('.email, .password').addClass('error')

  $('.login-form .submit').on 'click', ->
    $('.email, .password').removeClass('error')

  $('button.new-user').on 'click', ->
    document.location = "/admin/users/new"

  $('button.new-plan').on 'click', ->
    document.location = "/admin/plans/new"

  $("body").on 'click', '#modal-dismiss', (event) ->
    $.modal.close()

  $(document).on 'form-validation-error', (event) ->
    $target = $(event.fieldSelector)
    $targetContainer = $target.closest("div")

    $target.addClass "error"
    $targetContainer.append $("<div />").addClass("validation-error").text(event.error)

  $(document).on 'click', 'form .submit', (event) ->
    $(".validation-error").remove()
    $("input.error").removeClass("error")

  $(document).on 'redirecting-to-wepay', (event) ->
    nodes = [
      $("<h1 />")
        .text("Redirecting for Payment"),

      $("<p />")
        .text("You will now be redirected to WePay to enter your credit card information
                and authorize a preapproval so we can withdraw your subscription fee monthly.
                After you've successfully completed that process, you'll be redirected back
                here."),

      $("<button />")
        .attr("id", "modal-dismiss")
        .text("Continue to Payment")
    ]

    $("input.submit").attr("disabled", "disabled")
    modal("wepay-redirect-modal", nodes, (_) ->
      document.location = event.preapprovalUrl
    )

  $(document).on 'update-billing-information', (event) ->
    stripeCallback = (status, response) ->
      if response.error
        anchortabSite.validationError("#card-number", response.error.message)
      else
        updateStripeTokenFn = eval("(" + event.updateStripeTokenFn + ")")
        updateStripeTokenFn(response.id)

    nodes = [
      $("<h1 />")
        .text("Update Billing Information"),

      $("<p />")
        .text("Please enter your new billing information below."),

      $("<form />")
      .addClass("billing-information-form")
      .addClass("stripe-form")
      .append(
        $("<input />")
          .attr("type", "hidden")
          .attr("id", "stripe-token")
      )
      .append(
        $("<div />")
        .append(
          $("<label />")
            .attr("for", "card-number")
            .text("Card number:")
        )
        .append(
          $("<input />")
            .attr("type", "text")
            .attr("id", "card-number")
            .attr("placeholder", "0000 0000 0000 0000")
        )
      )
      .append(
        $("<div />")
        .append(
          $("<label />")
            .attr("for", "card-cvc")
            .text("CVC:")
        )
        .append(
          $("<input />")
            .attr("type", "text")
            .attr("id", "card-cvc")
            .attr("placeholder", "000")
            .attr("autocomplete", "off")
        )
      )
      .append(
        $("<div />")
        .append(
          $("<label />")
            .attr("for", "card-expiry")
            .text("Expires:")
        )
        .append(
          $("<input />")
            .attr("type", "text")
            .attr("id", "card-expiry")
            .attr("placeholder", "MM / YY")
        )
      )
      .append(
        $("<div />")
        .append(
          $("<label />")
            .attr("for", "card-billing-zip")
            .text("Billing ZIP:")
        )
        .append(
          $("<input />")
            .attr("type", "text")
            .attr("id", "card-billing-zip")
            .attr("placeholder", "00000")
        )
      )
      .append(
        $("<input />")
          .attr("type", "submit")
          .attr("class", "submit")
          .attr("value", "Update")
          .click((event) ->
            event.preventDefault()
            event.stopPropagation()

            anchortabSite.event("validate-stripe-form",
              stripeCallback: stripeCallback
            )
          )
      )
    ]

    modal("update-billing-modal", nodes)
    anchortabSite.event("stripe-form-ready")

  $(document).on 'tab-embed-code-received', (event) ->
    nodes = [
      $("<h1 />")
        .text("Embed Code"),

      $("<p />")
        .text("Below is the embed code for your tab. Add this code to your site
                just above the </body> tag to add your Anchor Tab to your beautiful
                piece of internet real estate."),

      $("<textarea></textarea>")
        .attr("readonly", "readonly")
        .text(event.embedCode),

      $("<p />")
        .append("Confused? ")
        .append($("<a />").attr("href", "mailto:hello@anchortab.com").text("Email us!")),

      $("<button />")
        .attr("id", "modal-dismiss")
        .text("Close")
    ]

    modal("embed-code-modal", nodes)

  $(document).on 'new-tab-created', (event) ->
    nodes = [
      $("<h1 />")
        .text("Tab Created"),

      $("<p />")
        .text("Congratulations on your shiny new tab. In the milliseconds since you pressed
                save, we've had the mechanics of your tab hooked together by the finest
                puzzlemaker that Bantu has to offer."),

      $("<p />")
        .text("The next step is to add your tab to your site."),

      $("<p />")
        .text("Below is the embed code for your tab. Add this code to your site
                just above the </body> tag to add your Anchor Tab to your beautiful
                piece of internet real estate."),

      $("<textarea></textarea>")
        .attr("readonly", "readonly")
        .text(event.embedCode),

      $("<p />")
        .append("Confused? ")
        .append($("<a />").attr("href", "mailto:hello@anchortab.com").text("Email us!")),

      $("<button />")
        .attr("id", "modal-dismiss")
        .text("Ok, cool.")
    ]

    modal("embed-code-modal", nodes, (event) ->
      document.location = "/manager/tabs"
    )
