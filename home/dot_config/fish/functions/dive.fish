function dive -d "List files and folders and dive into the selected one"
  set selected (fd $argv[1] | peco)
  if test -f "$selected"
    cd (dirname "$selected")
  else
    cd "$selected"
  end
end
