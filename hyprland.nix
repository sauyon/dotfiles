{
  pkgs,
  config,
  edgeGap,
  hyprDpmsPhysical,
  ...
}:
let
  lib = pkgs.lib;
  inherit (lib.generators) mkLuaInline;

  # Hyprland's $variable mechanism is a Hyprlang-parser feature; the Lua config
  # backend has no equivalent, so resolve these in Nix and inline the values.
  mainMod = "SUPER";
  terminal = "warp-terminal";
  menu = "walker";

  # Bind helpers. In the Lua config every `bind` entry is `hl.bind(keys,
  # dispatcher, opts?)`, where keys join mods/keys with " + ".
  #
  # Two ways to render the dispatcher as a Lua expression via mkLuaInline:
  #   exec  - run a shell command. `hl.dsp.exec_cmd` is Hyprlang `exec`.
  #           (NB: `hl.dsp.exec_raw` is Hyprlang `execr` - run a *program*
  #           without a shell. Neither runs a dispatcher; dispatchers go
  #           through the structured `hl.dsp.*` API below.)
  #   dsp   - a structured dispatcher, e.g. hl.dsp.focus({ direction = "left" }),
  #           hl.dsp.window.close(), hl.dsp.window.move({ workspace = 1 }).
  # [=[ ... ]=] long brackets let shell commands keep their quotes and `]]`.
  exec = keys: cmd: {
    _args = [
      keys
      (mkLuaInline "hl.dsp.exec_cmd([=[${cmd}]=])")
    ];
  };
  execOpts = keys: cmd: opts: {
    _args = [
      keys
      (mkLuaInline "hl.dsp.exec_cmd([=[${cmd}]=])")
      opts
    ];
  };
  dsp = keys: luaExpr: {
    _args = [
      keys
      (mkLuaInline luaExpr)
    ];
  };
  # Structured dispatcher with bind options (mouse drag/resize need
  # { mouse = true }).
  dspOpts = keys: luaExpr: opts: {
    _args = [
      keys
      (mkLuaInline luaExpr)
      opts
    ];
  };

  wsKeys = [
    "1"
    "2"
    "3"
    "4"
    "5"
    "6"
    "7"
    "8"
    "9"
    "0"
  ];
  workspaceBinds = lib.imap1 (
    i: k: dsp "${mainMod} + ${k}" "hl.dsp.focus({ workspace = ${toString i} })"
  ) wsKeys;
  moveToWorkspaceBinds = lib.imap1 (
    i: k: dsp "${mainMod} + SHIFT + ${k}" "hl.dsp.window.move({ workspace = ${toString i} })"
  ) wsKeys;
in
{
  enable = true;
  package = pkgs.hyprland;
  configType = "lua";
  settings = {
    config = {
      general = {
        gaps_in = 0;
        gaps_out = 0;
        border_size = 1;
      };

      input = {
        repeat_delay = 200;
        repeat_rate = 60;
        resolve_binds_by_sym = true;
      };

      animations.enabled = false;

      decoration = {
        shadow.enabled = false;
        blur.enabled = false;
      };

      misc = {
        disable_hyprland_logo = true;
        disable_splash_rendering = true;
        focus_on_activate = true;
        # Let a fresh hyprlock take over the lock if the original client dies
        # (e.g. when hms restarts hypridle.service and SIGTERMs the cgroup).
        allow_session_lock_restore = true;
      };
    };

    monitor = [
      {
        output = "HDMI-A-1";
        mode = "3840x2160@120";
        position = "0x0";
        scale = 1;
        bitdepth = 10;
        cm = "srgb";
      }
      {
        output = "";
        mode = "preferred";
        position = "auto";
        scale = 1;
      }
    ];

    workspace_rule = {
      workspace = "m[HDMI-A-1]";
      gaps_out = [
        0
        edgeGap
        edgeGap
        edgeGap
      ];
    };

    env = {
      _args = [
        "WARP_ENABLE_WAYLAND"
        "1"
      ];
    };

    device = [
      {
        name = "framework-laptop-16-keyboard-module---ansi-keyboard";
        kb_layout = "us";
        kb_variant = "colemak";
        kb_options = "ctrl:nocaps,ctrl:swapcaps";
      }
      {
        name = "at-translated-set-2-keyboard";
        kb_layout = "us";
        kb_variant = "colemak";
        kb_options = "ctrl:nocaps,ctrl:swapcaps";
      }
    ];

    # exec-once equivalents. The module already emits a hyprland.start hook for
    # the systemd/D-Bus activation environment, so we only add our own programs.
    on = {
      _args = [
        "hyprland.start"
        (mkLuaInline ''
          function()
            hl.exec_cmd("mako")
            hl.exec_cmd("hypr-fullscreen-inhibit")
            hl.exec_cmd("elephant")
          end'')
      ];
    };

    bind =
      [
        (exec "${mainMod} + return" terminal)
        (dsp "${mainMod} + SHIFT + W" "hl.dsp.window.close()")
        (exec "${mainMod} + SHIFT + E" "hyprland-graceful-exit")
        (dsp "${mainMod} + SHIFT + semicolon" ''hl.dsp.window.float({ action = "toggle" })'')
        # Jump focus to the other layer (floating <-> tiled). Needs a runtime
        # check, so it shells out; under the Lua config backend the IPC dispatch
        # must be a Lua expression (`focuswindow X` -> hl.dsp.focus({ window = X })).
        (exec "${mainMod} + semicolon" ''if [[ $(hyprctl activewindow -j | jq .floating) == true ]]; then t=tiled; else t=floating; fi; hyprctl dispatch "hl.dsp.focus({ window = \"$t\" })"'')
        (exec "${mainMod} + space" menu)
        (exec "ALT + space" menu)
        # Guard with `pidof hyprlock ||` (same as hypridle's lock_cmd): a second
        # hyprlock instance fights the first over the Goodix fingerprint device
        # ("Device already claimed"), which breaks fingerprint AND password auth
        # on the lock you're typing into. Never stack a duplicate.
        (exec "${mainMod} + SHIFT + Q" "pidof hyprlock || ${config.programs.hyprlock.package}/bin/hyprlock")
        (exec "${mainMod} + SHIFT + S" "${pkgs.hyprshot}/bin/hyprshot -m region")
        (exec "${mainMod} + E" "emacsclient -c")

        (exec "${mainMod} + O" "makoctl dismiss --all")

        (dsp "${mainMod} + B" ''hl.dsp.focus({ direction = "left" })'')
        (dsp "${mainMod} + F" ''hl.dsp.focus({ direction = "right" })'')
        (dsp "${mainMod} + P" ''hl.dsp.focus({ direction = "up" })'')
        (dsp "${mainMod} + N" ''hl.dsp.focus({ direction = "down" })'')
        (dsp "${mainMod} + SHIFT + B" ''hl.dsp.window.move({ direction = "left" })'')
        (dsp "${mainMod} + SHIFT + F" ''hl.dsp.window.move({ direction = "right" })'')
        (dsp "${mainMod} + SHIFT + P" ''hl.dsp.window.move({ direction = "up" })'')
        (dsp "${mainMod} + SHIFT + N" ''hl.dsp.window.move({ direction = "down" })'')

        (dsp "${mainMod} + SHIFT + left" ''hl.dsp.workspace.move({ monitor = "l" })'')
        (dsp "${mainMod} + SHIFT + right" ''hl.dsp.workspace.move({ monitor = "r" })'')
        (dsp "${mainMod} + SHIFT + up" ''hl.dsp.workspace.move({ monitor = "u" })'')
        (dsp "${mainMod} + SHIFT + down" ''hl.dsp.workspace.move({ monitor = "d" })'')

        (dsp "${mainMod} + M" "hl.dsp.window.fullscreen()")
      ]
      ++ workspaceBinds
      ++ moveToWorkspaceBinds
      ++ [
        (dsp "${mainMod} + mouse_down" ''hl.dsp.focus({ workspace = "e-1" })'')
        (dsp "${mainMod} + mouse_up" ''hl.dsp.focus({ workspace = "e+1" })'')

        # Mouse drag/resize (Hyprlang bindm).
        (dspOpts "${mainMod} + mouse:272" "hl.dsp.window.drag()" { mouse = true; })
        (dspOpts "${mainMod} + mouse:273" "hl.dsp.window.resize()" { mouse = true; })

        # Volume/brightness: repeat while held and work on the lock screen
        # (Hyprlang bindel).
        (execOpts "XF86AudioRaiseVolume" "wpctl set-volume -l 1 @DEFAULT_AUDIO_SINK@ 5%+" {
          locked = true;
          repeating = true;
        })
        (execOpts "XF86AudioLowerVolume" "wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-" {
          locked = true;
          repeating = true;
        })
        (execOpts "XF86AudioMute" "wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle" {
          locked = true;
          repeating = true;
        })
        (execOpts "XF86AudioMicMute" "wpctl set-mute @DEFAULT_AUDIO_SOURCE@ toggle" {
          locked = true;
          repeating = true;
        })
        (execOpts "XF86MonBrightnessUp" "${pkgs.brightnessctl}/bin/brightnessctl -e4 -n2 set 5%+" {
          locked = true;
          repeating = true;
        })
        (execOpts "XF86MonBrightnessDown" "${pkgs.brightnessctl}/bin/brightnessctl -e4 -n2 set 5%-" {
          locked = true;
          repeating = true;
        })

        # Media and lid switch: work on the lock screen (Hyprlang bindl).
        (execOpts "XF86AudioNext" "${pkgs.playerctl}/bin/playerctl next" { locked = true; })
        (execOpts "XF86AudioPause" "${pkgs.playerctl}/bin/playerctl play-pause" { locked = true; })
        (execOpts "XF86AudioPlay" "${pkgs.playerctl}/bin/playerctl play-pause" { locked = true; })
        (execOpts "XF86AudioPrev" "${pkgs.playerctl}/bin/playerctl previous" { locked = true; })

        (execOpts "switch:on:Lid Switch"
          # Lua IPC dispatch: `dispatch exec X` -> hl.dsp.exec_cmd("X").
          # `pidof hyprlock ||` so a lid-close on top of an existing lock (or one
          # already firing from suspend) can't stack a second instance that then
          # fights over the fingerprint device. DPMS-off always runs regardless.
          "${hyprDpmsPhysical} off && ( pidof hyprlock || hyprctl dispatch 'hl.dsp.exec_cmd(\"${config.programs.hyprlock.package}/bin/hyprlock\")' )"
          { locked = true; }
        )
        (execOpts "switch:off:Lid Switch" "${hyprDpmsPhysical} on" { locked = true; })
      ];
  };
}
