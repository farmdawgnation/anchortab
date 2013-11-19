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
  mixpanel.track("Viewed Landing Page")

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

  $('body').on 'click', 'button.plans-and-pricing', ->
    mixpanel.track("Click Plans & Pricing")

    $targetElem = $("section.plans-and-pricing > h3")
    targetTop = $targetElem.offset().top
    scrollableElem = scrollableElement('html', 'body')

    $(scrollableElem).animate({scrollTop: targetTop}, 600)

  plansAndPricingHasBeenViewed = false
  $(window).on 'scroll', ->
    unless plansAndPricingHasBeenViewed
      heightOfElemsAbovePricing = $(".im-on-a-boat").outerHeight() + $(".how-it-works").outerHeight() + $(".features").outerHeight()
      currentBottomScrollIndex = $(window).scrollTop() + $(window).innerHeight()

      if (currentBottomScrollIndex >= heightOfElemsAbovePricing)
        plansAndPricingHasBeenViewed = true
        mixpanel.track("Viewed Plans & Pricing")

  $(".go-to-manager").on 'click', ->
    document.location = "/manager/dashboard"

  $('.login').on 'click', '.submit', (event) ->
    $(event.target).closest(".login").find("input").removeClass("error")

  $(document).on 'login-failed', ->
    $(".login input").addClass("error")

  $(".register-button").on 'click', (event) ->
    document.location.href = "/register#" + $(event.target).data("plan-id")
