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

  $(document).on 'tab-embed-code-received', (event) ->
    $("#embed-code-modal").remove()

    embedModal =
      $("<div />")
        .addClass("modal")
        .attr("id", "embed-code-modal")
        .append(
          $("<h1 />")
            .text("Embed Code")
        )
        .append(
          $("<p />")
            .text("Below is your embed code for your tab. Add this code to your site
                    just above the </body> tag to add your Anchor Tab to your beautiful
                    piece of internet real estate.")
        )
        .append(
          $("<textarea></textarea>")
            .attr("readonly", "readonly")
            .text(event.embedCode)
        )
        .append(
          $("<p />")
            .append("Confused? ")
            .append($("<a />").attr("href", "mailto:hello@anchortab.com").text("Email us!"))
        )

    embedModal.on $.modal.CLOSE, (event) ->
      $(event.target).remove()

    $("body").append(embedModal)
    embedModal.modal()
