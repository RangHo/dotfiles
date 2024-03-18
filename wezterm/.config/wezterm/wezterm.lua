local wezterm = require 'wezterm'

return {
   -- Window settings
   window_background_opacity = 0.8,
   window_padding = {
      left = 15,
      right = 15,
      top = 15,
      bottom = 15,
   },
   hide_tab_bar_if_only_one_tab = true,

   -- Font-related
   font = wezterm.font_with_fallback {
      'semteulche',
      'Noto Sans Mono',
      'Symbols Nerd Font Mono',
   },
   font_size = 14.0,
}
