local M = {}
local wezterm = require("wezterm")

M.setup = function()
  wezterm.on("window-config-reloaded", function(window, _)
    window:toast_notification("wezterm", "configuration reloaded!", nil, 4000)
  end)
end

return M
