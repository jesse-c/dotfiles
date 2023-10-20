local wezterm = require 'wezterm'

-- Start
local config = wezterm.config_builder()

-- Typography
local font = require('font')

config.font_size = font.size

-- Appearance
local appearance = require('appearance')

config.enable_tab_bar = appearance.enable_tab_bar
config.warn_about_missing_glyphs = appearance.warn_about_missing_glyphs
config.color_scheme = appearance.scheme_for_appearance(wezterm.gui.get_appearance())
config.window_padding = appearance.window_padding

-- Keyboard
local keyboard = require('keyboard')

config.keys = keyboard.keys

-- Meta
require('meta').setup()

-- End
return config
