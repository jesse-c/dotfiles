-- Window management -----------------------------------------------------------
-- TODO Replace Rectangle to reduce third-party tool count

require("hs.window")
hs.window.animationDuration = 0

-- Window Switcher
switcher = hs.window.switcher.new(hs.window.filter.new():setCurrentSpace(true):setDefaultFilter())
switcher.ui.showThumbnails = false

hs.hotkey.bind("alt", "tab", "Next window", function()
  switcher:next()
end)
hs.hotkey.bind("alt-shift", "tab", "Prev window", function()
  switcher:previous()
end)

-- Resize a window to the percentage size of the total screen
function resizeWindow(window, percentOfScreen)
  local screen = window:screen()
  local frame = screen:frame()
  local size = {
    w = (frame.w * percentOfScreen) - (12*2),
    h = (frame.h * percentOfScreen) - (12*2),
  }
  window:setSize(size)
end

-- Bind a hotkey to centre the frontmost window
hs.hotkey.bind({ "cmd", "alt" }, "v", function()
  local window = hs.window.frontmostWindow()
  if window then
    resizeWindow(window, 0.75)
    window:centerOnScreen()
  end
end)

-- Bind a hotkey to maximise the frontmost window
hs.hotkey.bind({ "cmd", "alt" }, "f", function()
  local window = hs.window.frontmostWindow()
  if window then
    resizeWindow(window, 1)
    window:centerOnScreen()
  end
end)

hs.hotkey.bind({ "cmd", "alt" }, "c", function()
  local window = hs.window.frontmostWindow()
  if window then
    window:centerOnScreen()
  end
end)
