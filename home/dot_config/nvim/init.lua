-- Setup -----------------------------------------------------------------------

local userDir = "user"

require(userDir .. ".external")

-- User ------------------------------------------------------------------------

require("user.common")
require("user.ui")
-- require("user.os")
-- require("user.vcs")
-- require("user.editor")
-- require("user.lsp")
-- require("user.tree-sitter")
-- require("user.projects")
require("user.spelling")
require("user.modal")
-- require("user.notes")
-- require("user.completion")
require("user.file-system")
-- require("user.snippets")
-- require("user.lang-all")
-- require("user.lang-xxxx")

require("user.keymaps")
require("user.buffers")
require("user.abbrs")
-- require("user.spotlight")
require("user.undo")
require("user.search")
require("user.terminal")
require("user.clipboard")
-- require("user.folds")
-- require("user.tests")
