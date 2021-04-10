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
singleapps = {
  {'t', '/Applications/Things3.app'},
  {'k', 'kitty'},
  {'f', 'Firefox Nightly'},
  {'m', 'MailMate'},
  {'a', 'Safari'},
  {'p', 'Spotify'},
  {'w', 'Paw'},
  {'z', 'zoom.us'},
  {'s', 'Slack'},
  {'n', 'Notion'},
  {'r', 'Raycast'},
  {'d', 'Dash'},
}

for i, app in ipairs(singleapps) do
  k:bind({}, app[1], function() launch(app[2]); k:exit(); end)
end

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

