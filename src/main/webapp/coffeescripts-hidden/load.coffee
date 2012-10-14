###
Anchor Tab Loader
Release 0.1
(c) 2012 Matt Farmer and Cirion Group.
All Rights Reserved
###

loadScript = (url, callback) ->
  script = document.createElement("script")
  script.async = true
  script.type = "text/javascript"
 
  if script.readyState # IE
    script.onreadystatechange = ->
      if script.readyState == "loaded" || script.readyState == "complete"
        script.onreadystatechange = null
        callback()
  else # Others
    script.onload = ->
      callback();
 
  script.src = url
  document.getElementsByTagName("head")[0].appendChild(script)

withGoogleAnalytics = (callback) ->
  if ! _gaq
    analyticsSrc = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js'
    loadScript(analyticsSrc, callback)
  else
    callback()

withJQueryLoaded = (callback) ->
  if ! jQuery
    loadScript("http://ajax.googleapis.com/ajax/libs/jquery/1.8.2/jquery.min.js", callback)
  else if ! /^1.[678]/.test(jQuery.fn.jquery)
    console?.error("AnchorTab is disabled because you are using an unsupported version of jQuery.")

    # Track this on GA so we can see if we need to adjust this value over time.
    withGoogleAnalytics ->
      _gaq.push ["anchortab._trackEvent", "Bootstrap", "Incompatible jQuery Failure", jQuery.fn.jquery]

  else
    callback()

loadAnchorTab = ->
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
  withGoogleAnalytics ->
    _gaq.push ["anchortab._setAccount", "UA-35269224-2"]
    _gaq.push ["anchortab._setDomainName", "embed.anchortab.com"]
    _gaq.push ["anchortab._trackPageview"]

  withJQueryLoaded(loadAnchorTab)

  oldOnload?()
