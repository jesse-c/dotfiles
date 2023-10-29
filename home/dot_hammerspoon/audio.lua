-- AutoVolume -----------------------------------------------------------------

function sleepWatch(eventType)
  if eventType == hs.caffeinate.watcher.systemWillSleep or eventType == hs.caffeinate.watcher.systemDidWake then
    hs.audiodevice.defaultOutputDevice():setVolume(0)
  end
end

sleepWatcher = hs.caffeinate.watcher.new(sleepWatch)
sleepWatcher:start()
