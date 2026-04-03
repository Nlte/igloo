local wezterm = require 'wezterm'
local config = {}

-- WSL startup
config.default_domain = "WSL:Ubuntu-24.04"

-- Font
config.font = wezterm.font_with_fallback({
  { 
    family = 'JetBrains Mono', 
    -- We disable 'calt' (contextual), 'liga' (standard), and 'clig' (connection)
    -- Setting these to 0 explicitly overrides the font's defaults
    harfbuzz_features = { 'calt=0', 'clig=0', 'liga=0' } 
  },
})
config.font_size = 12.5
config.line_height = 1.0

-- Colors
config.color_scheme = "nordfox"

-- Window appearance
config.enable_tab_bar = true
config.use_fancy_tab_bar = false
config.window_decorations = "RESIZE"

config.window_padding = {
  left = 5,
  right = 5,
  top = 5,
  bottom = 5,
}

-- Performance
config.max_fps = 120
config.animation_fps = 120
config.front_end = "WebGpu"

-- Cursor
-- config.default_cursor_style = "BlinkingBlock"
-- config.cursor_blink_rate = 600

-- Scrollback
config.scrollback_lines = 10000

-- Keybindings
config.keys = {

  -- Pane splits
  {
    key = "-",
    mods = "ALT",
    action = wezterm.action.SplitVertical { domain = "CurrentPaneDomain" },
  },
  {
    key = "#",
    mods = "ALT",
    action = wezterm.action.SplitHorizontal { domain = "CurrentPaneDomain" },
  },

  -- Close pane
  {
    key = "x",
    mods = "ALT",
    action = wezterm.action.CloseCurrentPane { confirm = false },
  },

  -- Fullscreen pane
  {
    key = "Enter",
    mods = "ALT",
    action = wezterm.action.TogglePaneZoomState,
  },

  -- Vim-style pane navigation
  {
    key = "h",
    mods = "ALT",
    action = wezterm.action.ActivatePaneDirection("Left"),
  },
  {
    key = "j",
    mods = "ALT",
    action = wezterm.action.ActivatePaneDirection("Down"),
  },
  {
    key = "k",
    mods = "ALT",
    action = wezterm.action.ActivatePaneDirection("Up"),
  },
  {
    key = "l",
    mods = "ALT",
    action = wezterm.action.ActivatePaneDirection("Right"),
  },

  -- Resize panes
  {
    key = "H",
    mods = "ALT|SHIFT",
    action = wezterm.action.AdjustPaneSize { "Left", 5 },
  },
  {
    key = "J",
    mods = "ALT|SHIFT",
    action = wezterm.action.AdjustPaneSize { "Down", 5 },
  },
  {
    key = "K",
    mods = "ALT|SHIFT",
    action = wezterm.action.AdjustPaneSize { "Up", 5 },
  },
  {
    key = "L",
    mods = "ALT|SHIFT",
    action = wezterm.action.AdjustPaneSize { "Right", 5 },
  },

  -- New tab
  {
    key = "t",
    mods = "ALT",
    action = wezterm.action.SpawnTab "CurrentPaneDomain",
  },

  -- Close tab
  {
    key = "w",
    mods = "ALT",
    action = wezterm.action.CloseCurrentTab { confirm = false },
  },

  -- Quick launcher
  {
    key = "p",
    mods = "ALT",
    action = wezterm.action.ShowLauncher,
  },
  
  -- Neovim
  {
	key = "'",
	mods = "CTRL",
	action = wezterm.action.SendKey { key = "'", mods = "CTRL" }
  }
	

}

return config