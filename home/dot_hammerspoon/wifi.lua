wifiWatcher = nil
homeSSID = "DADF Hyperoptic 1GB Fibre 5Ghz"
workSSIDs = { "Duffel Crew" }
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
