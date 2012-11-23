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
  $('.footer-navigation a')
    .click(false)
    .tipsy
      gravity: 'w'
      fade: true
      opacity: 0.9

  $('body').on 'click', '.get-started-now', ->
    $targetElem = $("section.plans-and-pricing")
    targetTop = $targetElem.offset().top
    scrollableElem = scrollableElement('html', 'body')

    $(scrollableElem).animate({scrollTop: targetTop}, 600)

  getTwitters 'tweet-container',
    id: 'anchortab',
    template: '<p>%text%</p><p class="attribution"><a href="http://twitter.com/%user_screen_name%/statuses/%id_str%/">%time%</a> &middot; @%user_screen_name%</p>'
