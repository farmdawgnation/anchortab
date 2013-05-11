# use the first element that is "scrollable"
# http://css-tricks.com/snippets/jquery/smooth-scrolling/
scrollableElement = (els) ->
  for el in arguments
    $scrollElement = $(el)

    if $scrollElement.scrollTop() > 0
      return el
    else
      $scrollElement.scrollTop(1)
      isScrollable = $scrollElement.scrollTop() > 0
      $scrollElement.scrollTop(0)
      if isScrollable
        return el

  return [];

$(document).ready ->
  $('span[rel=tipsy]').tipsy({fade: true})

  window.liftAjax.lift_actualAjaxCall = (data, version, onSuccess, onFailure) ->
    hostnamePrefix = ""

    # In production, we want all AJAX calls from the landing (which is only login atm)
    # to be transmitted via HTTPS to protect user passwords.
    if window.location.hostname == "anchortab.com"
      hostnamePrefix = "https://anchortab.com"

    jQuery.ajax
      url : hostnamePrefix + liftAjax.addPageNameAndVersion("/ajax_request/", version),
      data : data,
      dataType : "jsonp",
      success : onSuccess,
      error : onFailure,
      crossDomain: true

  $('body').on 'click', '.plans-and-pricing', ->
    mixpanel.track("Click Plans & Pricing")

    $targetElem = $("section.plans-and-pricing > h3")
    targetTop = $targetElem.offset().top
    scrollableElem = scrollableElement('html', 'body')

    $(scrollableElem).animate({scrollTop: targetTop}, 600)

  $(".go-to-manager").on 'click', ->
    document.location = "/manager/dashboard"

  $('.login').on 'click', '.submit', (event) ->
    $(event.target).closest(".login").find("input").removeClass("error")

  $(document).on 'login-failed', ->
    $(".login input").addClass("error")

  $(".enterprise-action-button").on 'click', (event) ->
    mixpanel.track("Click Enrprise Signup Button")
    document.location.href = "mailto:hello@anchortab.com?subject=Enterprise Edition"

  $(".register-button").on 'click', (event) ->
    document.location.href = "/register#" + $(event.target).data("plan-id")

  getTwitters 'tweet-container',
    id: 'anchortab',
    template: '<p>%text%</p><p class="attribution"><a href="http://twitter.com/%user_screen_name%/statuses/%id_str%/">%time%</a> &middot; @%user_screen_name%</p>'
