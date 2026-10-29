-- Allow Alfred to run the Hammerspoon workflow's commands via AppleScript.
hs.allowAppleScript(true)

require("console")
require("ipc")
require("wifi")
require("hyper")
require("audio")
require("media")
require("mouse")
require("windows")
require("mail")
require("appearance")

-- Machine-local config at ~/.hammerspoon/local.lua, kept out of this repo and
-- loaded only if present (mirrors ~/.config/fish/local.fish).
if hs.fs.attributes(hs.configdir .. "/local.lua") then
  require("local")
end
