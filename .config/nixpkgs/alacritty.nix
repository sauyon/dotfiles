{
  enable = true;
  settings = {
    # Configuration for Alacritty, the GPU enhanced terminal emulator.


    # Any items in the `env` entry below will be added as
    # environment variables. Some entries may override variables
    # set by alacritty itself.
    env = {
    # TERM variable
    #
    # This value is used to set the `$TERM` environment variable for
    # each instance of Alacritty. If it is not present, alacritty will
    # check the local terminfo database and use 'alacritty' if it is
    # available, otherwise 'xterm-256color' is used.
      TERM = "alacritty-direct";
    };

    window = {
      # Window dimensions (changes require restart)
      #
      # Specified in number of columns/lines, not pixels.
      # If both are `0`, this setting is ignored.
      dimensions = { columns = 0; lines = 0; };


      # Window position (changes require restart)
      #
      # Specified in number of pixels.
      # If the position is not set, the window manager will handle the placement.
      # position = { x = 0; y = 0; }

      # Window padding (changes require restart)
      #
      # Blank space added around the window in pixels. This padding is not scaled
      # by DPI and the specified value is always added at both opposing sides.
      padding = { x = 0; y = 0; };

      # Spread additional padding evenly around the terminal content.
      #dynamic_padding = false;


      # Window decorations
      #
      # Values for `decorations`:
      #     - full: Borders and title bar
      #     - none: Neither borders nor title bar
      decorations = "full";

      # Startup Mode (changes require restart)
      #
      # Values for `startup_mode`:
      #   - Windowed
      #   - Maximized
      #   - Fullscreen
      #startup_mode: Windowed

      # Window title
      #title: Alacritty

      # Window class (Linux only):
      #class:
      # Application instance name
      #instance: Alacritty
      # General application class
      #general: Alacritty

      # GTK theme variant (Linux only)
      #
      # Override the variant of the GTK theme. Commonly supported values are `dark` and `light`.
      # Set this to `None` to use the default theme variant.
      #gtk_theme_variant: None
    };

    scrolling = {
      # Maximum number of lines in the scrollback buffer.
      # Specifying '0' will disable scrolling.
      history = 10000;

      # Number of lines the viewport will move for every line scrolled when
      # scrollback is enabled (history > 0).
      multiplier = 3;
    };

    # Font configuration
    #
    # Important font attributes like antialiasing, subpixel aa, and hinting can be
    # controlled through fontconfig. Specifically, the following attributes should
    # have an effect:
    #   - hintstyle
    #   - antialias
    #   - lcdfilter
    #   - rgba
    #
    # For instance, if you wish to disable subpixel antialiasing, you might set the
    # rgba property to `none`. If you wish to completely disable antialiasing, you
    # can set antialias to `false`.
    #
    # Please see these resources for more information on how to use fontconfig:
    #   - https://wiki.archlinux.org/index.php/font_configuration#Fontconfig_configuration
    #   - file:///usr/share/doc/fontconfig/fontconfig-user.html
    font = {
      # Normal (roman) font face
      normal.family = "Noto Mono for Powerline";
      # normal.style = "Regular";
      # Bold font face
      bold.family = "Noto Mono for Powerline";
      # Italic font face
      italic.family = "Noto Mono for Powerline";
      # Bolt italic font face
      bold_italic.famil = "Noto Mono for Powerline";

      # Point size
      size = 12.0;

      # Offset is the extra space around each character. `offset.y` can be thought of
      # as modifying the line spacing, and `offset.x` as modifying the letter spacing.
      offset = { x = 0; y = 0; };
      # Glyph offset determines the locations of the glyphs within their cells with
      # the default being at the bottom. Increasing `x` moves the glyph to the right,
      # increasing `y` moves the glyph upwards.
      glyph_offset = { x = 0; y = 0; };
    };

    # If `true`, bold text is drawn using the bright color variants.
    draw_bold_text_with_bright_colors = true;

    # Colors (Solarized Dark)
    colors = {
      primary = {
        background = "0x002b36";
        foreground = "0x839496";
      };

      normal = {
        black = "0x073642";
        red = "0xdc322f";
        green  = "0x859900";
        yellow  = "0xb58900";
        blue = "0x268bd2";
        magenta = "0xd33682";
        cyan = "0x2aa198";
        white = "0xeee8d5";
      };

      bright  = {
        black = "0x002b36";
        red = "0xcb4b16";
        green = "0x586e75";
        yellow = "0x657b83";
        blue = "0x839496";
        magenta = "0x6c71c4";
        cyan = "0x93a1a1";
        white = "0xfdf6e3";
      };
    };

    # Visual Bell
    #
    # Any time the BEL code is received, Alacritty "rings" the visual bell. Once
    # rung, the terminal background will be set to white and transition back to the
    # default background color. You can control the rate of this transition by
    # setting the `duration` property (represented in milliseconds). You can also
    # configure the transition function by setting the `animation` property.
    #
    # Values for `animation`:
    #   - Ease
    #   - EaseOut
    #   - EaseOutSine
    #   - EaseOutQuad
    #   - EaseOutCubic
    #   - EaseOutQuart
    #   - EaseOutQuint
    #   - EaseOutExpo
    #   - EaseOutCirc
    #   - Linear
    #
    # Specifying a `duration` of `0` will disable the visual bell.
    visual_bell = { animation = "EaseOutExpo"; duration = 1; };

    # Background opacity
    #
    # Window opacity as a floating point number from `0.0` to `1.0`.
    # The value `0.0` is completely transparent and `1.0` is opaque.
    background_opacity = 1.0;

    selection = {
      semantic_escape_chars = ",│`|:\"' ()[]{}<>\t";

      # When set to `true`, selected text will be copied to both the primary and
      # the selection clipboard. Otherwise, it will only be copied to the selection
      # clipboard.
      save_to_clipboard = false;
    };

    dynamic_title = true;

    cursor = {
      # Cursor style
      #
      # Values for 'cursor.style':
      #   - ▇ Block
      #   - _ Underline
      #   - | Beam
      style = "Block";

      # If this is `true`, the cursor will be rendered as a hollow box when the
      # window is not focused.
      unfocused_hollow = true;
    };

    # Live config reload (changes require restart)
    live_config_reload = true;

    # Shell
    #
    # You can set `shell.program` to the path of your favorite shell, e.g. `/bin/fish`.
    # Entries in `shell.args` are passed unmodified as arguments to the shell.
    #
    #shell:
    #  program: /bin/bash
    #  args:
    #    - --login


    # Startup directory
    #
    # Directory the shell is started in. If this is unset, or `None`, the working
    # directory of the parent process will be used.
    #working_directory: None


    # Send ESC (\x1b) before characters when alt is pressed.
    #alt_send_esc: true

    debug = {
      # Display the time it takes to redraw each frame.
      #render_timer: false

      # Keep the log file after quitting Alacritty.
      #persistent_logging: false

      # Log level
      #
      # Values for `log_level`:
      #   - None
      #   - Error
      #   - Warn
      #   - Info
      #   - Debug
      #   - Trace
      #log_level: Warn
    };

    # Print all received window events.
    #print_events: false

    # Record all characters and escape sequences as test data.
    #ref_test: false

    mouse = {
      # Click settings
      #
      # The `double_click` and `triple_click` settings control the time
      # alacritty should wait for accepting multiple clicks as one double
      # or triple click.
      double_click = { threshold = 300; };
      triple_click = { threshold = 300; };

      # If this is `true`, the cursor is temporarily hidden when typing.
      hide_when_typing = false;

      url = {
        # URL launcher
        #
        # This program is executed when clicking on a text which is recognized as a URL.
        # The URL is always added to the command as the last parameter.
        #
        # When set to `None`, URL launching will be disabled completely.
        #
        # Default:
        #   - (macOS) open
        #   - (Linux) xdg-open
        #   - (Windows) explorer
        #launcher:
        #  program: xdg-open
        #  args: []

        # URL modifiers
        #
        # These are the modifiers that need to be held down for opening URLs when clicking
        # on them. The available modifiers are documented in the key binding section.
        modifiers = "None";
      };
    };

    # Mouse bindings
    #
    # Mouse bindings are specified as a list of objects, much like the key
    # bindings further below.
    #
    # Each mouse binding will specify a:
    #
    # - `mouse`:
    #
    #   - Middle
    #   - Left
    #   - Right
    #   - Numeric identifier such as `5`
    #
    # - `action` (see key bindings)
    #
    # And optionally:
    #
    # - `mods` (see key bindings)
    mouse_bindings = [
      { mouse = "Middle"; action = "PasteSelection"; }
    ];

    # Key bindings
    #
    # Key bindings are specified as a list of objects. For example, this is the
    # default paste binding:
    #
    # `- { key: V, mods: Control|Shift, action: Paste }`
    #
    # Each key binding will specify a:
    #
    # - `key`: Identifier of the key pressed
    #
    #    - A-Z
    #    - F1-F24
    #    - Key0-Key9
    #
    #    A full list with available key codes can be found here:
    #    https://docs.rs/glutin/*/glutin/enum.VirtualKeyCode.html#variants
    #
    #    Instead of using the name of the keys, the `key` field also supports using
    #    the scancode of the desired key. Scancodes have to be specified as a
    #    decimal number. This command will allow you to display the hex scancodes
    #    for certain keys:
    #
    #       `showkey --scancodes`.
    #
    # Then exactly one of:
    #
    # - `chars`: Send a byte sequence to the running application
    #
    #    The `chars` field writes the specified string to the terminal. This makes
    #    it possible to pass escape sequences. To find escape codes for bindings
    #    like `PageUp` (`"\x1b[5~"`), you can run the command `showkey -a` outside
    #    of tmux. Note that applications use terminfo to map escape sequences back
    #    to keys. It is therefore required to update the terminfo when changing an
    #    escape sequence.
    #
    # - `action`: Execute a predefined action
    #
    #   - Copy
    #   - Paste
    #   - PasteSelection
    #   - IncreaseFontSize
    #   - DecreaseFontSize
    #   - ResetFontSize
    #   - ScrollPageUp
    #   - ScrollPageDown
    #   - ScrollLineUp
    #   - ScrollLineDown
    #   - ScrollToTop
    #   - ScrollToBottom
    #   - ClearHistory
    #   - Hide
    #   - Quit
    #   - ToggleFullscreen
    #   - SpawnNewInstance
    #   - ClearLogNotice
    #   - ReceiveChar
    #   - None
    #
    #   (macOS only):
    #   - ToggleSimpleFullscreen: Enters fullscreen without occupying another space
    #
    # - `command`: Fork and execute a specified command plus arguments
    #
    #    The `command` field must be a map containing a `program` string and an
    #    `args` array of command line parameter strings. For example:
    #       `{ program: "alacritty", args: ["-e", "vttest"] }`
    #
    # And optionally:
    #
    # - `mods`: Key modifiers to filter binding actions
    #
    #    - Command
    #    - Control
    #    - Option
    #    - Super
    #    - Shift
    #    - Alt
    #
    #    Multiple `mods` can be combined using `|` like this:
    #       `mods: Control|Shift`.
    #    Whitespace and capitalization are relevant and must match the example.
    #
    # - `mode`: Indicate a binding for only specific terminal reported modes
    #
    #    This is mainly used to send applications the correct escape sequences
    #    when in different modes.
    #
    #    - AppCursor
    #    - AppKeypad
    #    - Alt
    #
    #    A `~` operator can be used before a mode to apply the binding whenever
    #    the mode is *not* active, e.g. `~Alt`.
    #
    # Bindings are always filled by default, but will be replaced when a new
    # binding with the same triggers is defined. To unset a default binding, it can
    # be mapped to the `ReceiveChar` action. Alternatively, you can use `None` for
    # a no-op if you do not wish to receive input characters for that binding.
    key_bindings = [
      { key = "V";        mods = "Control|Shift";    action = "Paste";                }
      { key = "C";        mods = "Control|Shift";    action = "Copy";                 }
      { key = "Paste";                      action = "Paste";                         }
      { key = "Copy";                       action = "Copy";                          }
      { key = "L";        mods = "Control"; action = "ClearLogNotice";                }
      { key = "L";        mods = "Control"; chars = "\\x0c";                          }
      { key = "Insert";   mods = "Shift";   action = "PasteSelection";                }
      { key = "Key0";     mods = "Control"; action = "ResetFontSize";                 }
      { key = "Equals";   mods = "Control"; action = "IncreaseFontSize";              }
      { key = "Subtract"; mods = "Control"; action = "DecreaseFontSize";              }
      { key = "PageUp";   mods = "Shift";   action = "ScrollPageUp";   mode = "~Alt"; }
      { key = "PageDown"; mods = "Shift";   action = "ScrollPageDown"; mode = "~Alt"; }
      { key = "Home";     mods = "Shift";   action = "ScrollToTop";    mode = "~Alt"; }
      { key = "End";      mods = "Shift";   action = "ScrollToBottom"; mode = "~Alt"; }
    ];
  };
}
