local wezterm = require("wezterm")

local M = {}

M.keys = {
  { key = "h", mods = "SUPER", action = wezterm.action({ SendString = "\x02\x68" }) },
  { key = "j", mods = "SUPER", action = wezterm.action({ SendString = "\x02\x6a" }) },
  { key = "k", mods = "SUPER", action = wezterm.action({ SendString = "\x02\x6b" }) },
  { key = "l", mods = "SUPER", action = wezterm.action({ SendString = "\x02\x6c" }) },
  { key = "t", mods = "SUPER", action = wezterm.action({ SendString = "\x02\x63" }) },
  { key = "w", mods = "SUPER", action = wezterm.action({ SendString = "\x02\x78" }) },
  { key = "d", mods = "SUPER", action = wezterm.action({ SendString = "\x02\x22" }) },
  { key = "d", mods = "SUPER|SHIFT", action = wezterm.action({ SendString = "\x02\x25" }) },
  { key = "h", mods = "SUPER|SHIFT", action = wezterm.action({ SendString = "\x02\x70" }) },
  { key = "l", mods = "SUPER|SHIFT", action = wezterm.action({ SendString = "\x02\x6e" }) },
  { key = "r", mods = "SUPER", action = wezterm.action({ SendString = "\x02\x2c" }) },
  { key = "0", mods = "SUPER", action = wezterm.action({ SendString = "\x02\x27" }) },
  { key = "1", mods = "SUPER", action = wezterm.action({ SendString = "\x02\x31" }) },
  { key = "2", mods = "SUPER", action = wezterm.action({ SendString = "\x02\x32" }) },
  { key = "3", mods = "SUPER", action = wezterm.action({ SendString = "\x02\x33" }) },
  { key = "4", mods = "SUPER", action = wezterm.action({ SendString = "\x02\x34" }) },
  { key = "5", mods = "SUPER", action = wezterm.action({ SendString = "\x02\x35" }) },
  { key = "6", mods = "SUPER", action = wezterm.action({ SendString = "\x02\x36" }) },
  { key = "7", mods = "SUPER", action = wezterm.action({ SendString = "\x02\x37" }) },
  { key = "8", mods = "SUPER", action = wezterm.action({ SendString = "\x02\x38" }) },
  { key = "9", mods = "SUPER", action = wezterm.action({ SendString = "\x02\x39" }) },
}

return M
