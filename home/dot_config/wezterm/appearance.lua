local M = {}

M.dark = "Catppuccin Mocha"
M.light = "Catppuccin Latte"
M.scheme_for_appearance = function(appearance)
  if appearance:find "Dark" then
    return M.dark
  else
    return M.light
  end
end
M.enable_tab_bar = false
M.warn_about_missing_glyphs = false
local pd = '1px'
M.window_padding = {
  left = pd,
  right = pd,
  top = pd,
  bottom = pd,
}

return M
