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
