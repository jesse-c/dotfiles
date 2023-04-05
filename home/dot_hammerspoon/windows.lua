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

-- Set the percentage of screen to use
local percentOfScreen = 0.75

-- Function to center a window on the screen
function centerWindow(window)
	local screen = window:screen()
	local frame = screen:frame()
	local size = {
		w = frame.w * percentOfScreen,
		h = frame.h * percentOfScreen,
	}
	local origin = {
		x = frame.x + ((frame.w - size.w) / 2),
		y = frame.y + ((frame.h - size.h) / 2),
	}
	window:setSize(size)
	window:setTopLeft(origin)
end

-- Bind a hotkey to centre the frontmost window
hs.hotkey.bind({ "cmd", "alt" }, "v", function()
	local window = hs.window.frontmostWindow()
	if window then
		centerWindow(window)
	end
end)
