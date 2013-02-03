window.anchortabSite =
  event: (eventName, parameters) ->
    event = jQuery.Event(eventName, parameters)
    jQuery(document).trigger event

$(document).ready ->
  liftComet?.lift_sessionLost = ->
    document.location.reload()
