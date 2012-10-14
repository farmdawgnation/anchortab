###
Anchor Tab Loader
Release 0.1
(c) 2012 Matt Farmer and Cirion Group.
All Rights Reserved
###

includeAnalyticsIfNeeded = ->
  if ! _gaq
    # Ported from Google's embed code.
    ga = document.createElement 'script'
    ga.type = 'text/javascript'
    ga.async = true
    ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js'
    s = document.getElementsByTagName 'head'
    s[0].appendChild ga

includeJQueryIfNeeded = ->
  if ! jQuery
    # Pull down jQuery from GooleAPIs
    jqueryUri = "http://ajax.googleapis.com/ajax/libs/jquery/1.8.2/jquery.min.js"

    jq = document.createElement 'script'
    jq.type = 'text/javascript'
    jq.src = jqueryUri
    s = document.getElementsByTagName 'head'
    s[0].appendChild jq
    true
  else if ! /^1.[678]/.test(jQuery.fn.jquery)
    console?.error("AnchorTab is disabled. Please update your version of jQuery.")
    false

anchorTabLoader = ->
  # Load the anchor tab. At this point we can assume that jQuery exists and that we're able to use it
  # safely without things getting hairy. If we find out in the future that things are wonky about that
  # then we'll have to refactor some of this code, I suppose.
  $ = jQuery # In case we're in compability mode.

  # Load the Anchor Tab stylesheet.
  atStyleSheet =
    $("<link />")
      .attr("href", "http://embed.anchortab.com/stylesheets/tab.css")
      .attr("rel", "stylesheet")
      .attr("type", "text/css")

  $("head").append atStyleSheet

  # Create the tab and append to the end of the body.
  anchorTab =
    $("<div />")
      .attr("id", "anchor-tab")
      .append(
        $("<p />").text("This is an anchor tab.")
      )

  $("body").append anchorTab

# Do the neccicary mojo to make sure we load up our goods after the
# window is loaded. This includes making sure all our dependancies
# are in place.
oldOnload = window.onload
window.onload = ->
  includeAnalyticsIfNeeded()

  # We only proceed to load the tab if the jQuery load was successful.
  # If we found an ancient version of jQuery on the page, we bunk and don't
  # load the tab.
  if includeJQueryIfNeeded()
    anchorTabLoader()

  oldOnload?()
