-- Move mouse cursor ----------------------------------------------------------

local next = 0
local prev = 1

local mouseMoveMods = { "cmd", "ctrl" }

local function moveMouseToScreen(direction)
  local screen = hs.mouse.getCurrentScreen()

  local destinationScreen
  if direction == next then
    destinationScreen = screen:next()
  else
    destinationScreen = screen:previous()
  end

  local rect = destinationScreen:fullFrame()
  local center = hs.geometry.rectMidPoint(rect)

  hs.mouse.setAbsolutePosition(center)
end

local function moveMouseToNextScreen()
  moveMouseToScreen(next)
end

local function moveMouseToPrevScreen()
  moveMouseToScreen(prev)
end

hs.hotkey.bind(mouseMoveMods, "h", moveMouseToPrevScreen)
hs.hotkey.bind(mouseMoveMods, "l", moveMouseToNextScreen)
