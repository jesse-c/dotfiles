-- Config ----------------------------------------------------------------------
-- Console
hs.console.darkMode(true)

hs.loadSpoon("ReloadConfiguration")
spoon.ReloadConfiguration:start()

-- Init ------------------------------------------------------------------------
-- TODO Module loading pattern

-- Wifi ------------------------------------------------------------------------
wifiWatcher = nil
homeSSID = "DADF Hyperoptic 1GB Fibre 5Ghz"
workSSIDs = {"Duffel Crew"}
lastSSID = hs.wifi.currentNetwork()
wifiMenu = hs.menubar.new()

function ssidMenuMessage(newSSID)
  if newSSID then
    return newSSID
  else 
    return "Not connected"
  end
end

wifiMenu:setTitle(ssidMenuMessage(hs.wifi.currentNetwork()))

function ssidChangedCallback()
  local newSSID = hs.wifi.currentNetwork()

  -- AutoVolume
  -- Automatically set the volume to 0 when leaving the home wifi and when the
  -- machine goes to sleep.
  if newSSID == homeSSID and lastSSID ~= homeSSID then
    -- We just joined our home WiFi network
  elseif newSSID ~= homeSSID and lastSSID == homeSSID then
    -- We just departed our home WiFi network
  elseif newSSID ~= homeSSID then
    -- We just joined a new WiFi network that isn't our home one
    hs.audiodevice.defaultOutputDevice():setVolume(0)
  end

  -- When at work
  for _, v in ipairs(workSSIDs) do
      if v == newSSID then
        -- Do nothing
      end
  end

  lastSSID = newSSID

  -- Show SSID in menu bar -----------------------------------------------------
  wifiMenu:setTitle(ssidMenuMessage(newSSID))
end

wifiWatcher = hs.wifi.watcher.new(ssidChangedCallback)
wifiWatcher:start()

-- Quick application switching -------------------------------------------------

-- https://liuhao.im/english/2017/06/02/macos-automation-and-shortcuts-with-hammerspoon.html

local function open(name)
    return function()
        hs.application.launchOrFocus(name)
        if name == 'Finder' then
            hs.appfinder.appFromName(name):activate()
        end
    end
end

hs.hotkey.bind({"alt"}, "t", open("/Applications/Things3.app"))
hs.hotkey.bind({"alt"}, "k", open("kitty"))
hs.hotkey.bind({"alt"}, "f", open("Firefox Nightly"))
hs.hotkey.bind({"alt"}, "m", open("MailMate"))
hs.hotkey.bind({"alt"}, "a", open("Safari"))
hs.hotkey.bind({"alt"}, "p", open("Spotify"))
hs.hotkey.bind({"alt"}, "w", open("Paw"))
hs.hotkey.bind({"alt"}, "z", open("zoom.us"))
hs.hotkey.bind({"alt"}, "s", open("Slack"))
hs.hotkey.bind({"alt"}, "n", open("Notion"))

-- Move mouse cursor ----------------------------------------------------------

local next = 0
local prev = 1

local mouseMoveMods = {'cmd', 'ctrl'}

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

hs.hotkey.bind(mouseMoveMods, 'h', moveMouseToPrevScreen)
hs.hotkey.bind(mouseMoveMods, 'l', moveMouseToNextScreen)

-- AutoVolume -----------------------------------------------------------------

function sleepWatch(eventType)
  if (eventType == hs.caffeinate.watcher.systemWillSleep or eventType == hs.caffeinate.watcher.systemDidWake) then
    hs.audiodevice.defaultOutputDevice():setVolume(0)
  end
end

sleepWatcher = hs.caffeinate.watcher.new(sleepWatch)
sleepWatcher:start()

-- Currently playing song ------------------------------------------------------

local spotifyMenu = hs.menubar.new()

local spotifyLogger = hs.logger.new('spotifyMenu','debug')
spotifyLogger.i('Initializing')

local function hideSpotifyMenu()
  if spotifyMenu:isInMenuBar() then
    spotifyMenu:removeFromMenuBar()
    spotifyMenu:setTitle("")
  end
end

local function showSpotifyMenu()
  if not spotifyMenu:isInMenuBar() then
    spotifyMenu:returnToMenuBar()
  end
end

local function updateSpotifyMenu()
  spotifyLogger.i('callback')
  if hs.spotify.isRunning() then
    if hs.spotify.getPlaybackState() == hs.spotify.state_playing then
      spotifyLogger.i('playing')
      local currentArtist = hs.spotify.getCurrentArtist()
      local currentTrack = hs.spotify.getCurrentTrack()
      spotifyMenu:setTitle(currentArtist .. " — " .. currentTrack)

      showSpotifyMenu()
    else
      spotifyLogger.i('not playing')
      hideSpotifyMenu()
    end
  else
    hideSpotifyMenu()
  end
end

-- DISABLED hs.timer.doEvery(1, updateSpotifyMenu)

-- Window management -----------------------------------------------------------
-- TODO Replace Spectacle

-- Window Switcher
switcher = hs.window.switcher.new(
  hs.window.filter.new():setCurrentSpace(true):setDefaultFilter()
)
switcher.ui.showThumbnails = false

hs.hotkey.bind('alt', 'tab', 'Next window', function() switcher:next() end)
hs.hotkey.bind('alt-shift', 'tab', 'Prev window',function() switcher:previous() end)

-- Mail ------------------------------------------------------------------------
-- mail = hs.image.imageFromPath("~/.hammerspoon/bin/mail.png")
-- mail:setSize({w=16,h=16})
-- mailMenu = hs.menubar.new()
-- mailMenu:setIcon(mail)

-- function updateMailMenu()
--   local succeeded, result, desc = hs.osascript.applescript("tell application \"Mail\" to get the unread count of inbox")

--   if (succeeded and result ~= nil) then
--     local count = tostring(result)
--     mailMenu:setTitle(count)
--   end

--   local getUnreadList = [[
--     tell application "Mail"
--       set msgs to (get the messages 1 thru 10 of inbox)
--       set unreadMsgsIDs to {}
      
--       repeat with msg in msgs
--         if read status of msg is false then
--           set end of unreadMsgsIDs to (get id of msg)
--         end if
--       end repeat
--     end tell

--     return unreadMsgsIDs
--   ]]
--   local succeeded, result, desc = hs.osascript.applescript(getUnreadList)

--   if (succeeded and result ~= nil) then
--     local subjects = {}

--     for _,id in pairs(result) do
--       local readFn = function(id) 
--         return function (keys, item)
--           -- TODO Do something after run
--           local _, _, _ = hs.osascript.applescript("tell application \"Mail\" to set read status of (first message of inbox whose id is equal to " .. id .. ") to true")
--         end
--       end

--       local deleteFn = function(id)
--         return function (keys, item)
--           -- TODO Do something after run
--           local _, _, _ = hs.osascript.applescript("tell application \"Mail\" to delete (first message of inbox whose id is equal to " .. id .. ")")
--         end
--       end
      
--       local succeededSubject, subject, _ = hs.osascript.applescript("tell application \"Mail\" to get subject of (first message of inbox whose id is equal to " .. id .. ")")
--       local succeededSender, sender, _ = hs.osascript.applescript("tell application \"Mail\" to get sender of (first message of inbox whose id is equal to " .. id .. ")")
--       -- TODO Date sent

--       if (succeededSubject and subject ~= nil and succeededSender and sender ~= nil) then
--         local msgMenu = {}
--         table.insert(msgMenu, {title = "Delete", fn = deleteFn(id)})
--         table.insert(msgMenu, {title = "Mark as read", fn = readFn(id)})

--         table.insert(subjects, {title = sender .. " — " .. subject, menu = msgMenu})
--       end
--     end

--     mailMenu:setMenu(subjects)
--   end
-- end

-- updateMailMenu()
-- hs.timer.doEvery(1, updateMailMenu)
