-- Config ----------------------------------------------------------------------
-- Console
hs.console.darkMode(true)
-- Global config reload
hs.hotkey.bind({"cmd", "alt", "ctrl"}, "R", function()
  -- TODO hs.alert.show("Config reloaded")
  hs.reload()
end)

-- Init ------------------------------------------------------------------------
-- TODO Module loading pattern

-- Wifi ------------------------------------------------------------------------
wifiWatcher = nil
homeSSID = "NETGEAR97"
workSSIDs = {"jazz", "TechnoWLAN"}
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

jnConnect = [[
  tell application "Viscosity"
    connect "JN"
  end
]]

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

  -- Connect to VPN when at work -
  for _, v in ipairs(workSSIDs) do
      if v == newSSID then
        _, _, _ = hs.osascript.applescript(jnConnect)
      end
  end

  lastSSID = newSSID

  -- Show SSID in menu bar -----------------------------------------------------
  wifiMenu:setTitle(ssidMenuMessage(newSSID))
end

wifiWatcher = hs.wifi.watcher.new(ssidChangedCallback)
wifiWatcher:start()

-- AutoVolume ------------------------------------------------------------------

function sleepWatch(eventType)
  if (eventType == hs.caffeinate.watcher.systemWillSleep or eventType == hs.caffeinate.watcher.systemDidWake) then
    hs.audiodevice.defaultOutputDevice():setVolume(0)
  end
end

sleepWatcher = hs.caffeinate.watcher.new(sleepWatch)
sleepWatcher:start()

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
mail = hs.image.imageFromPath("~/.hammerspoon/bin/mail.png")
mail:setSize({w=16,h=16})
mailMenu = hs.menubar.new()
mailMenu:setIcon(mail)

function updateMailMenu()
  local succeeded, result, desc = hs.osascript.applescript("tell application \"Mail\" to get the unread count of inbox")

  if (succeeded and result ~= nil) then
    local count = tostring(result)
    mailMenu:setTitle(count)
  end

  local getUnreadList = [[
    tell application "Mail"
      set msgs to (get the messages 1 thru 10 of inbox)
      set unreadMsgsIDs to {}
      
      repeat with msg in msgs
        if read status of msg is false then
          set end of unreadMsgsIDs to (get id of msg)
        end if
      end repeat
    end tell

    return unreadMsgsIDs
  ]]
  local succeeded, result, desc = hs.osascript.applescript(getUnreadList)

  if (succeeded and result ~= nil) then
    local subjects = {}

    for _,id in pairs(result) do
      local readFn = function(id) 
        return function (keys, item)
          -- TODO Do something after run
          local _, _, _ = hs.osascript.applescript("tell application \"Mail\" to set read status of (first message of inbox whose id is equal to " .. id .. ") to true")
        end
      end

      local deleteFn = function(id)
        return function (keys, item)
          -- TODO Do something after run
          local _, _, _ = hs.osascript.applescript("tell application \"Mail\" to delete (first message of inbox whose id is equal to " .. id .. ")")
        end
      end
      
      local succeededSubject, subject, _ = hs.osascript.applescript("tell application \"Mail\" to get subject of (first message of inbox whose id is equal to " .. id .. ")")
      local succeededSender, sender, _ = hs.osascript.applescript("tell application \"Mail\" to get sender of (first message of inbox whose id is equal to " .. id .. ")")
      -- TODO Date sent

      if (succeededSubject and subject ~= nil and succeededSender and sender ~= nil) then
        local msgMenu = {}
        table.insert(msgMenu, {title = "Delete", fn = deleteFn(id)})
        table.insert(msgMenu, {title = "Mark as read", fn = readFn(id)})

        table.insert(subjects, {title = sender .. " — " .. subject, menu = msgMenu})
      end
    end

    mailMenu:setMenu(subjects)
  end
end

updateMailMenu()
hs.timer.doEvery(5, updateMailMenu)
