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
