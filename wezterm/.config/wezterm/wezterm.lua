local wezterm = require 'wezterm'
local mux = wezterm.mux

local config = wezterm.config_builder()

config.color_scheme = 'Solarized (dark) (terminal.sexy)'
config.window_decorations = 'NONE'
config.default_prog = { '/usr/bin/fish', '-l' }

wezterm.on('gui-startup', function(cmd)
  local tab, pane, window = mux.spawn_window(cmd or {})
  window:gui_window():maximize()
end)

return config
