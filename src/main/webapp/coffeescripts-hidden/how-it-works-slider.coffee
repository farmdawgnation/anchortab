#
# How It Works Slider for Landing Page
#
currentSlide = 0
slideStripViewWidth = 0
numberOfSlides = 4
slideWidth = 725

changePageIndicatorTo = (page) ->
  $("ol.pages li").removeClass 'selected'

  classname = "slide-" + page
  $("ol.pages li." + classname).addClass 'selected'

nextSlide = ->
  # Ignore if already on the last slide.
  return false if currentSlide == numberOfSlides - 1

  # Track MP
  mixpanel.track("Click Next Slide")

  currentSlide = currentSlide + 1
  offset = (slideWidth * currentSlide) * -1

  $('.slide-strip').css 'left', offset
  changePageIndicatorTo(currentSlide + 1)

  if currentSlide == (numberOfSlides - 1)
    $('.slide-next').css 'display', 'none'

  $('.slide-previous').css 'display', 'block'

  return false

previousSlide = ->
  # Ignore if already on first slide.
  return false if currentSlide == 0

  # Track MP
  mixpanel.track("Click Previous Slide")

  currentSlide = currentSlide - 1
  offset = (slideWidth * currentSlide) * -1

  $('.slide-strip').css 'left', offset
  changePageIndicatorTo(currentSlide + 1)

  if currentSlide == 0
    $('.slide-previous').css 'display', 'none'

  $('.slide-next').css 'display', 'block'

  return false

jumpToSlide = (event) ->
  page = event.target.hash.replace("#", "")

  # something funky
  return false if page < 1 || page > numberOfSlides

  # Track MP
  mixpanel.track("Jump to page", page: page)

  currentSlide = page - 1
  offset = (slideWidth * currentSlide) * -1

  $(".slide-strip").css 'left', offset
  changePageIndicatorTo(page)

  if currentSlide == 0
    $('.slide-previous').css 'display', 'none'
  else
    $('.slide-previous').css 'display', 'block'

  if currentSlide == (numberOfSlides - 1)
    $('.slide-next').css 'display', 'none'
  else
    $('.slide-next').css 'display', 'block'

  return false

$(document).on 'ready', ->
  slideStripViewWidth = $('.slide-strip-view').width()
  numberOfSlides = $('.slide').length

  $('.slide-strip-navigator')
    .on('click', '.slide-previous', previousSlide)
    .on('click', '.slide-next', nextSlide)

  $('ol.pages').on('click', 'a', jumpToSlide)
