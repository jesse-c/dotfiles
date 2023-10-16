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

return M
