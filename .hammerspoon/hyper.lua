-- require("hs.fnutils")

-- https://gist.github.com/ttscoff/cce98a711b5476166792d5e6f1ac5907

-- A global variable for the Hyper Mode
-- Creates a sort of namespace
k = hs.hotkey.modal.new({}, "F17")

-- Trigger existing hyper key shortcuts
-- Example:
-- k:bind({}, 'm', nil, function() hs.eventtap.keyStroke({"cmd","alt","shift","ctrl"}, 'm') end)

-- Build your own
launch = function(appname)
  hs.application.launchOrFocus(appname)
  if name == 'Finder' then
      hs.appfinder.appFromName(name):activate()
  end
  k.triggered = true
end

-- Single keybinding for app launch
hs.fnutils.each({
  { key = 'q', appname = '/Applications/Things3.app'},
  { key = 'g', appname = 'kitty'},
  { key = 'f', appname = 'Firefox Nightly'},
  { key = 'm', appname = 'Mail'},
  { key = 'a', appname = 'Safari'},
  { key = 'p', appname = 'Spotify'},
  { key = 'w', appname = 'Paw'},
  { key = 'z', appname = 'zoom.us'},
  { key = 's', appname = 'Slack'},
  { key = 'n', appname = 'Notion'},
  { key = 'r', appname = 'Raycast'},
  { key = 'd', appname = 'Dash'},
  { key = 't', appname = 'TablePlus'},
}, function(object)
  k:bind({}, object.key, function() launch(object.appname); k:exit(); end)
end)

-- Shortcut to reload config
ofun = function()
  hs.reload()
  hs.alert.show("Config loaded")
  k.triggered = true
end
k:bind({}, 'o', nil, ofun)

-- Enter Hyper Mode when F18 (Hyper/Capslock) is pressed
pressedF18 = function()
  k.triggered = false
  k:enter()
end

-- Leave Hyper Mode when F18 (Hyper/Capslock) is pressed, send ESCAPE if no
-- other keys are pressed.
releasedF18 = function()
  k:exit()
  if not k.triggered then
    hs.eventtap.keyStroke({}, 'ESCAPE')
  end
end

-- Bind the Hyper key
f18 = hs.hotkey.bind({}, 'F18', pressedF18, releasedF18)

-- Window management -----------------------------------------------------------

-- Temporarily here. Ideally would be in windows.lua, but we need access to the
-- Hyper binding.

-- http://larryhynes.net/2015/04/a-minor-update-to-my-hammerspoon-config.html
k:bind({}, 'h', function() hs.window.focusedWindow():focusWindowWest(); k:exit(); end)
k:bind({}, 'j', function() hs.window.focusedWindow():focusWindowNorth(); k:exit(); end)
k:bind({}, 'k', function() hs.window.focusedWindow():focusWindowSouth(); k:exit(); end)
k:bind({}, 'l', function() hs.window.focusedWindow():focusWindowEast(); k:exit(); end)
