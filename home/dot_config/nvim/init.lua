-- Setup -----------------------------------------------------------------------

local userDir = "user"

require(userDir .. ".external")

-- User ------------------------------------------------------------------------

require("user.common")
require("user.ui")
require("user.spelling")
require("user.modal")
require("user.file-system")
require("user.buffers")
require("user.abbrs")
require("user.undo")
require("user.search")
require("user.terminal")
require("user.clipboard")
