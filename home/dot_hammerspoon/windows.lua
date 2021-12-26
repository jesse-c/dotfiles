-- Window management -----------------------------------------------------------
-- TODO Replace Rectangle

require("hs.window")
hs.window.animationDuration = 0

-- Window Switcher
switcher = hs.window.switcher.new(
  hs.window.filter.new():setCurrentSpace(true):setDefaultFilter()
)
switcher.ui.showThumbnails = false

hs.hotkey.bind('alt', 'tab', 'Next window', function() switcher:next() end)
hs.hotkey.bind('alt-shift', 'tab', 'Prev window',function() switcher:previous() end)

