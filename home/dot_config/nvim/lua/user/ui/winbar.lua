local winbar = {}

winbar.eval = function()
  if vim.api.nvim_eval_statusline("%f", {})["str"] == "[No Name]" then
    return ""
  end
  -- Full path: "%f"
  return vim.fn.pathshorten(vim.fn.fnamemodify(vim.fn.expand("%:p"), ":p:."))
end

return winbar
