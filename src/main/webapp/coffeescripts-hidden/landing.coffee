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
  $('body').on 'click', '.get-started-now', ->
    $targetElem = $("section.plans-and-pricing")
    targetTop = $targetElem.offset().top
    scrollableElem = scrollableElement('html', 'body')

    $(scrollableElem).animate({scrollTop: targetTop}, 600)
