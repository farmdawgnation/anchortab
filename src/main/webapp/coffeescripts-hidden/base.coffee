window.anchortabSite =
  event: (eventName, parameters) ->
    event = jQuery.Event(eventName, parameters)
    jQuery(document).trigger event

  validationError: (fieldSelector, error) ->
    anchortabSite.event 'form-validation-error',
      fieldSelector: fieldSelector,
      error: error

$(document).ready ->
  liftComet?.lift_sessionLost = ->
    document.location.reload()
