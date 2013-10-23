###
Anchor Tab Loader
Release 0.1
(c) 2012 Matt Farmer and Cirion Group.
All Rights Reserved
###

##
## In dev and staging, we calculate the hostnames for the tab embed based
## on the hostname used to embed them. Since we serve the load script from
## S3 for production, we use a hack that sbt-resource-management gains us:
## the ability to mash two files together on deploy.
##
parser = document.createElement("a")
parser.href = document.getElementById("anchortab-loader").src

googleAnalyticsAccount = anchortab?.googleAnalyticsAccount || "UA-35269224-4"
apiDomain = anchortab?.apiDomain || parser.hostname
secureApiDomain = anchortab?.secureApiDomain || parser.hostname
resourcesDomain = anchortab?.resourcesDomain || parser.hostname
secureResourcesDomain = anchortab?.secureResourcesDomain || parser.hostname
jqVersion = "1.9.1"

window.anchortab = window.anchortab || {}
window.anchortab.i18n = {}

##
## LIB FUNCTIONS
##
setStateCookie = (state) ->
  nDays = 30
  today = new Date()
  expire = new Date()
  expire.setTime(today.getTime() + 3600000*24*nDays)
  document.cookie = "anchorTabState=" + state + ";path=/;expires=" + expire.toGMTString()

getStateCookie = ->
  cookieMatch = document.cookie.match(/anchorTabState=([a-z]+)(;)?/)
  cookieMatch?[1]

# http://stackoverflow.com/questions/901115/how-can-i-get-query-string-values
getParam = (name) ->
  results = new RegExp('[\\?&]' + name + '=([^&#]*)').exec(window.location.href)
  if ! results
    return 0;
  results[1] || 0

##
## BOOTSTRAPPING FUNCTIONS
##
window._gaq = window._gaq || []

loadScript = (url, callback) ->
  script = document.createElement("script")
  script.async = true
  script.type = "text/javascript"
 
  if script.readyState # IE
    script.onreadystatechange = ->
      if script.readyState == "loaded" || script.readyState == "complete"
        script.onreadystatechange = null
        callback?()
  else # Others
    script.onload = ->
      callback?();
 
  script.src = url
  document.getElementsByTagName("head")[0].appendChild(script)

loadGAIfNeeded = ->
  # Determine if GAQ was already loaded on the page by checking to see
  # If reduce is a valid method on it. If the real _gaq has been loaded,
  # it won't be an array, and this will return false.
  if _gaq.reduce?
    secure = 'https:' == document.location.protocol
    if secure
      domainRoot = 'https://ssl'
    else
      domainRoot = 'http://www'

    analyticsSrc = domainRoot + '.google-analytics.com/ga.js'

    # After the script is loaded, we need to make sure to reassign our local
    # _gaq to the _gaq loaded by Google Analytics.
    loadScript analyticsSrc

isAcceptableJQuery = ->
  jQueryVersionPieces = jQuery.fn.jquery.split(".")

  if (jQueryVersionPieces[1] >= 5) || (Number(jQueryVersionPieces[1]) == 4 && jQueryVersionPieces[2] >= 3)
    true
  else
    false

withJQueryLoaded = (callback) ->
  if ! jQuery?
    loadScript("//ajax.googleapis.com/ajax/libs/jquery/" + jqVersion + "/jquery.min.js", callback)
  else if ! isAcceptableJQuery()
    console?.error("AnchorTab is disabled because you are using an unsupported version of jQuery.")

    # Track this on GA so we can see if we need to adjust this value over time.
    _gaq.push ["at._trackEvent", "Bootstrap", "Incompatible jQuery Failure", jQuery.fn.jquery]

  else
    callback()

isBrowserIe = ->
  div = document.createElement("div")
  div.innerHTML = "<!--[if IE]><i></i><![endif]-->"
  isIe = (div.getElementsByTagName("i").length == 1)

  return isIe

isBrowserIe8OrLess = ->
  div = document.createElement("div")
  div.innerHTML = "<!--[if lt IE 9]><i></i><![endif]-->"
  isIeLessThan9 = (div.getElementsByTagName("i").length == 1)

  return isIeLessThan9

shouldMobilize = ->
  # We say that we should mobilize if the screen with is 320
  if screen?.width == 320
    true
  else
    false

##
## ANCHOR TAB LOADING AND DISPLAY
##
submitEmail = (event) ->
  # In case of compatibility mode
  $ = jQuery
  tabId = $("#anchortab-loader").data('tab-id')

  $target = $(event.target)
  $emailInput = $target.closest("#anchor-tab").find(".email-input")
  $firstNameInput = $target.closest("#anchor-tab").find(".first-name-input")

  email = $emailInput.val()
  name = $firstNameInput.val()

  unless email.match(/.+@.+\..+/)
    $("#anchor-tab")
      .find(".success-message")
        .text(anchortab.i18n["tab-invalidEmail"])
      .end()
      .addClass("success")

    setTimeout ->
      $("#anchor-tab")
        .removeClass("success")
    , 4000

    return

  $target.attr('disabled', 'disabled')
  $emailInput.attr('disabled', 'disabled')
  $firstNameInput.attr('disabled', 'disabled')

  submissionUri = "//" + apiDomain + "/api/v1/embed/" + tabId + "/submit"

  $.ajax
    url: submissionUri
    dataType: 'jsonp'
    headers:
      'X-Embedded-Domain': location.host
    data:
      email: email
      name: name
    success: (event) ->
      $("#anchor-tab")
        .find(".success-message")
          .text(event.message)
        .end()
        .addClass("success")
        .find(".email-input")
          .val("")

      gaqInfo = "Domain: " + document.domain
      _gaq.push ["at._trackEvent", "Submission", "Email Submitted", gaqInfo]

      setTimeout ->
        $("#anchor-tab")
          .removeClass("visible")
          .removeClass("success")
          .addClass("minimized")

        $("#anchor-tab [disabled]").removeAttr("disabled")

        setStateCookie "minimized"
      , 4000

    error: (xhr, status, error) ->
      $("#anchor-tab [disabled]").removeAttr("disabled")
      alert(anchortab.i18n["tab-somethingWentWrong"])

      gaqInfo = "Domain: " + document.domain + " Response Status: " + status
      _gaq.push ["at._trackEvent", "Submission", "Submission Error", gaqInfo]

displayTab = (tabJson) ->
  # In case of compatibility mode
  $ = jQuery

  # Pull out the various descriptors into useful information.
  font = tabJson.font
  displayDelay = tabJson.delay * 1000
  colorScheme = tabJson.colorScheme
  customMessage = tabJson.customText
  collectName = tabJson.collectName
  window.anchortab.i18n = tabJson.i18n

  # Load the Anchor Tab stylesheet.
  atStyleSheet =
    $("<link />")
      .attr("rel", "stylesheet")
      .attr("type", "text/css")

  if document.location.protocol == "https:"
    atStyleSheet.attr("href", "//" + secureResourcesDomain + "/stylesheets/tab.css")
  else
    atStyleSheet.attr("href", "//" + resourcesDomain + "/stylesheets/tab.css")

  $("head").append atStyleSheet

  anchorTabStamp = []

  if ! tabJson.whitelabel
    anchorTabStamp =
      $("<a />")
        .addClass("anchortab-stamp")
        .attr('href', "http://anchortab.com")
        .attr('target', '_blank')

  # Build the color scheme CSS. :/
  colorSchemeStyle = "background: #{colorScheme.baseColor}; "
  colorSchemeStyle += "background: -webkit-gradient(linear, 50% 0%, 50% 100%, color-stop(0%, #{colorScheme.baseColor}), color-stop(100%, #{colorScheme.secondaryColor}));"
  colorSchemeStyle += "background: -webkit-linear-gradient(#{colorScheme.baseColor}, #{colorScheme.secondaryColor}); "
  colorSchemeStyle += "background: -moz-linear-gradient(#{colorScheme.baseColor}, #{colorScheme.secondaryColor});"
  colorSchemeStyle += "background: -o-linear-gradient(#{colorScheme.baseColor}, #{colorScheme.secondaryColor});"
  colorSchemeStyle += "background: linear-gradient(#{colorScheme.baseColor}, #{colorScheme.secondaryColor});"

  # Create the tab and append to the end of the body.
  anchorTab =
    $("<div />")
      .attr("id", "anchor-tab")
      .attr("style", colorSchemeStyle)
      .append(anchorTabStamp)
      .append(
        $("<p />")
          .addClass("custom-message")
          .text(customMessage)
      )
      .append(
        $("<input />")
          .addClass('email-input')
          .attr('type', 'text')
          .attr('placeholder', anchortab.i18n["tab-emailAddress"])
      )
      .append(
        $("<button />")
          .addClass('email-submission')
          .text(anchortab.i18n["tab-submit"])
          .click(submitEmail)
      )
      .append(
        $("<button />")
          .addClass('minimize')
          .text(anchortab.i18n["tab-minimizeAnchorTab"])
          .click(->
              $("#anchor-tab").removeClass("visible").addClass("minimized")
              setStateCookie 'minimized'

              gaqInfo = "Domain: " + document.domain
              _gaq.push ["at._trackEvent", "Tab Visibility", "Minimize Tab", gaqInfo]
            )
      )
      .append(
        $("<button />")
          .addClass("maximize")
          .text(anchortab.i18n["tab-maximizeAnchorTab"])
          .click(->
              $("#anchor-tab").removeClass("minimized").addClass("visible")
              setStateCookie 'visible'

              gaqInfo = "Domain: " + document.domain
              _gaq.push ["at._trackEvent", "Tab Visibility", "Maximize Tab", gaqInfo]
            )
      )
      .append(
        $("<p />")
          .addClass("success-message")
          .text("Success!")
      )

  # We need a style tag to give the maximize button the
  # correct color.
  maximizeColorSelector = "#anchor-tab.desktop .maximize:before, #anchor-tab.desktop .maximize:after"
  maximizeColorStyle = "background-color: #{colorScheme.secondaryColor} !important;"

  if collectName
    anchorTab.find(".custom-message").after(
      $("<input />")
        .addClass("first-name-input")
        .attr("type", "text")
        .attr("placeholder", anchortab.i18n["tab-firstName"])
    )

  if isBrowserIe()
    if isBrowserIe8OrLess()
      return # Bail, we don't work in IE <= 8.

    _gaq.push ["at._trackEvent", "Bootstrap", "MSIE", navigator.userAgent]
    anchorTab.addClass "msie"

  if shouldMobilize()
    anchorTab.addClass("mobilized").addClass("minimized")

    if $("meta[name=viewport]").length > 0
      anchorTab.addClass("mobile-optimized-page")

    anchorTab.find(".maximize").text("Subscribe")
    anchorTab.find(".maximize").attr("style", colorSchemeStyle)

    anchorTab.find(".minimize").text("Close")
  else
    anchorTab.addClass "desktop"

  stylesheet = document.createElement("style")
  document.head.appendChild(stylesheet)

  if stylesheet.sheet.addRule?
    stylesheet.sheet.addRule?(maximizeColorSelector, maximizeColorStyle)
  else
    stylesheet.sheet.insertRule(maximizeColorSelector + " {" + maximizeColorStyle + "}", 0)

  $("body").append anchorTab

  unless shouldMobilize()
    setTimeout ->
      if getStateCookie() == 'minimized'
        anchorTab.addClass "minimized"
      else
        anchorTab.addClass "visible"
    , displayDelay

loadAnchorTab = ->
  # Load the anchor tab. At this point we can assume that jQuery exists and that we're able to use it
  # safely without things getting hairy. If we find out in the future that things are wonky about that
  # then we'll have to refactor some of this code, I suppose.
  $ = jQuery # In case we're in compability mode.

  # If the page we're on has done something incredibly stupid and runs this script twice, we need to
  # bail so we don't load two Anchor Tabs on one page.
  return if $("#anchor-tab").length > 0

  tabId = $("#anchortab-loader").data('tab-id')
  tabJson = "//" + apiDomain + "/api/v1/embed/" + tabId

  $.ajax
    url: tabJson
    dataType: 'jsonp'
    success: displayTab
    headers:
      'X-Embedded-Domain': location.host
    error: (xhr, status, error) ->
      console.error(error)

# Do the neccicary mojo to make sure we load up our goods after the
# window is loaded. This includes making sure all our dependancies
# are in place.
oldOnload = window.onload
window.onload = ->
  _gaq.push ['at._setAccount', googleAnalyticsAccount]
  _gaq.push ['at._setDomainName', 'anchortab.com']
  _gaq.push ['at._setAllowLinker', true]
  _gaq.push ["at._trackPageview"]

  loadGAIfNeeded()
  withJQueryLoaded(loadAnchorTab)

  oldOnload?()
