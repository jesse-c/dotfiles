" https://neovim.io/doc/user/lsp.html

" Go to definition
nmap <silent> gd <cmd>lua vim.lsp.buf.definition()<CR>

" Hover doc
" nmap <silent> K  <cmd>lua vim.lsp.buf.hover()<CR>
" Using Lspsaga
nnoremap <silent> K <cmd>lua require('lspsaga.hover').render_hover_doc()<CR>
" scroll down hover doc
nnoremap <silent> <C-f> <cmd>lua require('lspsaga.hover').smart_scroll_hover(1)<CR>
" scroll up hover doc
nnoremap <silent> <C-b> <cmd>lua require('lspsaga.hover').smart_scroll_hover(-1)<CR>

" Code action
nnoremap <silent><leader>ca <cmd>lua require('lspsaga.codeaction').code_action()<CR>
vnoremap <silent><leader>ca <cmd>'<,'>lua require('lspsaga.codeaction').range_code_action()<CR>

" Signature help
nnoremap <silent> gs <cmd>lua require('lspsaga.signaturehelp').signature_help()<CR>

" Rename
nnoremap <silent>gr <cmd>lua require('lspsaga.rename').rename()<CR>

" Preview definition
nnoremap <silent> ge <cmd>lua require'lspsaga.provider'.preview_definition()<CR>

" Jump Diagnostic and Show Diagnostics

"" Show
nnoremap <silent> <leader>cd <cmd>lua require'lspsaga.diagnostic'.show_line_diagnostics()<CR>
nnoremap <silent> <leader>cd :Lspsaga show_line_diagnostics<CR>

"" Jump diagnostic
nnoremap <silent> [e <cmd>lua require'lspsaga.diagnostic'.lsp_jump_diagnostic_prev()<CR>
nnoremap <silent> ]e <cmd>lua require'lspsaga.diagnostic'.lsp_jump_diagnostic_next()<CR>

" Float terminal
" Float terminal also you can pass the cli command in open_float_terminal function
" or open_float_terminal('lazygit')<CR>
nnoremap <silent> <A-d> <cmd>lua require('lspsaga.floaterm').open_float_terminal()<CR>
nnoremap <silent> <A-d> <C-\><C-n>:lua require('lspsaga.floaterm').close_float_terminal()<CR>
