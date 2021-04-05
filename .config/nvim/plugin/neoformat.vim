nnoremap <leader>d <cmd>Neoformat<cr>

augroup fmt
  autocmd!
  autocmd BufWritePre * undojoin | Neoformat
augroup END
