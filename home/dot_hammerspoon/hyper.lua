-- https://gist.github.com/ttscoff/cce98a711b5476166792d5e6f1ac5907

-- A global variable for the Hyper Mode
-- Creates a sort of namespace
local k = hs.hotkey.modal.new({}, nil)

-- Trigger existing hyper key shortcuts
-- Example:
-- k:bind({}, 'm', nil, function() hs.eventtap.keyStroke({"cmd","alt","shift","ctrl"}, 'm') end)

-- Build your own
local launch = function(appname)
  hs.application.launchOrFocus(appname)
  if appname == 'Finder' then
      hs.appfinder.appFromName(appname):activate()
  end
  k.triggered = true
end

-- Single keybinding for app launch
hs.fnutils.each(
   hs.json.read("~/.hammerspoon/hyper.json")["apps"],
   function(object)
  k:bind({}, object.key, function() launch(object.appname); k:exit(); end)
end)

-- Shortcut to reload config
local ofun = function()
  hs.reload()
  hs.alert.show("Config loaded")
  k.triggered = true
end
k:bind({}, 'o', nil, ofun)

-- Enter Hyper Mode when F18 (Hyper/Capslock) is pressed
local pressedF18 = function()
  k.triggered = false
  k:enter()
end

-- Leave Hyper Mode when F18 (Hyper/Capslock) is pressed, send ESCAPE if no
-- other keys are pressed.
local releasedF18 = function()
  k:exit()
  if not k.triggered then
    hs.eventtap.keyStroke({}, 'ESCAPE')
  end
end

-- Bind the Hyper key
hs.hotkey.bind({}, 'F18', pressedF18, releasedF18)

-- Window management -----------------------------------------------------------

-- Temporarily here. Ideally would be in windows.lua, but we need access to the
-- Hyper binding.

-- http://larryhynes.net/2015/04/a-minor-update-to-my-hammerspoon-config.html
k:bind({}, 'h', function() hs.window.focusedWindow():focusWindowWest(); k:exit(); end)
k:bind({}, 'j', function() hs.window.focusedWindow():focusWindowNorth(); k:exit(); end)
k:bind({}, 'k', function() hs.window.focusedWindow():focusWindowSouth(); k:exit(); end)
k:bind({}, 'l', function() hs.window.focusedWindow():focusWindowEast(); k:exit(); end)
