###
Anchor Tab Loader
Release 0.1
(c) 2012 Matt Farmer and Cirion Group.
All Rights Reserved
###

##
## Configuration - It'd be nice if we could deduce these from a props
## file for something, but in lieu of that, this works.
##
googleAnalyticsAccount = "UA-35269224-4"
apiDomain = "local.anchortab.com"
resourcesDomain = "local.anchortab.com"
jqVersion = "1.8.2"

##
## LIB FUNCTIONS
##
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

withJQueryLoaded = (callback) ->
  if ! jQuery?
    loadScript("http://ajax.googleapis.com/ajax/libs/jquery/" + jqVersion + "/jquery.min.js", callback)
  else if ! /^1.[678]/.test(jQuery.fn.jquery)
    console?.error("AnchorTab is disabled because you are using an unsupported version of jQuery.")

    # Track this on GA so we can see if we need to adjust this value over time.
    _gaq.push ["at._trackEvent", "Bootstrap", "Incompatible jQuery Failure", jQuery.fn.jquery]

  else
    callback()

##
## ANCHOR TAB LOADING AND DISPLAY
##
submitEmail = (event) ->
  # In case of compatibility mode
  $ = jQuery
  tabId = $("#anchortab-loader").data('tab-id')

  $target = $(event.target)
  $input = $target.closest("#anchor-tab").find(".email-input")
  email = $input.val()
  submissionUri = "http://" + apiDomain + "/api/v1/embed/" + tabId + "/submit"

  $.ajax
    url: submissionUri
    dataType: 'jsonp'
    data:
      email: email
    success: (event) ->
      alert("Email submitted!")
    error: (xhr, status, error) ->
      alert(error)

displayTab = (tabJson) ->
  # In case of compatibility mode
  $ = jQuery

  # Pull out the various descriptors into useful information.
  font = tabJson.font
  displayDelay = tabJson.delay * 1000
  colorScheme = tabJson.colorScheme.toLowerCase()

  # Load the Anchor Tab stylesheet.
  atStyleSheet =
    $("<link />")
      .attr("href", "http://" + resourcesDomain + "/stylesheets/tab.css")
      .attr("rel", "stylesheet")
      .attr("type", "text/css")

  $("head").append atStyleSheet

  # Create the tab and append to the end of the body.
  anchorTab =
    $("<div />")
      .attr("id", "anchor-tab")
      .addClass(colorScheme)
      .append(
        $("<a />")
          .addClass("anchortab-stamp")
          .attr('href', "http://anchortab.com")
          .attr('target', '_blank')
      )
      .append(
        $("<p />").text("This is an anchor tab.")
      )
      .append(
        $("<input />")
          .addClass('email-input')
          .attr('type', 'text')
          .attr('placeholder', "email address")
      )
      .append(
        $("<button />")
          .addClass('email-submission')
          .text("Submit")
          .click(submitEmail)
      )
      .append(
        $("<button />")
          .addClass('minimize')
          .text("Minimize Anchor Tab")
      )
      .append(
        $("<button />")
          .addClass("maximize")
          .text("Maximize Anchor Tab")
      )

  $("body").append anchorTab

  setTimeout ->
    anchorTab.addClass "visible"
  , displayDelay

  $("#anchor-tab")
    .on('click', '.minimize', ->
      $("#anchor-tab").removeClass("visible").addClass("minimized")
    )
    .on('click', '.maximize', ->
      $("#anchor-tab").removeClass("minimized").addClass("visible")
    )

loadAnchorTab = ->
  # Load the anchor tab. At this point we can assume that jQuery exists and that we're able to use it
  # safely without things getting hairy. If we find out in the future that things are wonky about that
  # then we'll have to refactor some of this code, I suppose.
  $ = jQuery # In case we're in compability mode.
  tabId = $("#anchortab-loader").data('tab-id')
  tabJson = "http://" + apiDomain + "/api/v1/embed/" + tabId

  $.ajax
    url: tabJson
    dataType: 'jsonp'
    success: displayTab
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
