return {
  {
    "mg979/vim-visual-multi",
    -- event = "BufRead",
    config = function()
      -- For some reason, this is only working within vim.cmd

      -- Test 1
      -- local mappings = {}
      -- mappings["Add Cursor Down"] = "<S-Down>"
      -- mappings["Add Cursor Up"] = "<S-Up>"
      -- mappings["Undo"] = "u"
      -- mappings["Redo"] = "<C-r>"
      --
      -- vim.g.VM_default_mappings = 0
      -- vim.g.VM_mouse_mappings = 0
      -- vim.g.VM_maps = mappings

      -- Test 2
      -- vim.g.VM_mouse_mappings = 0

      -- vim.g.VM_maps = {
      -- 	["Add Cursor Down"] = "<Down>",
      -- 	["Add Cursor Up"] = "<Up>",
      -- 	Undo = "u",
      -- 	Redo = "<C-r>",
      -- 	Exit = "<C-c>",
      -- }

      -- Test 3
      vim.cmd([[
			  let g:VM_default_mappings = 0
			  let g:VM_mouse_mappings = 0
			  let g:VM_maps = {}
			  let g:VM_maps["Add Cursor Down"] = '<S-Down>'
			  let g:VM_maps["Add Cursor Up"] = '<S-Up>'
			  let g:VM_maps["Undo"] = 'u'
			  let g:VM_maps["Redo"] = '<C-r>'
			  let g:VM_maps["Exit"] = '<C-c>'
			]])
    end,
  },
}
