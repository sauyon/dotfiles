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
  # dispatcher, opts?)`, where keys join mods/keys with " + ". We render the
  # dispatcher as a raw Lua expression via mkLuaInline:
  #   exec_cmd  - run a shell command (Hyprlang `exec`)
  #   exec_raw  - run a raw dispatcher string, like `hyprctl dispatch <...>`
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
  dispatch = keys: raw: {
    _args = [
      keys
      (mkLuaInline "hl.dsp.exec_raw([=[${raw}]=])")
    ];
  };
  # Raw Lua dispatcher with options (mouse drag/resize need { mouse = true }).
  bindLua = keys: luaExpr: opts: {
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
  workspaceBinds = lib.imap1 (i: k: dispatch "${mainMod} + ${k}" "workspace ${toString i}") wsKeys;
  moveToWorkspaceBinds = lib.imap1 (
    i: k: dispatch "${mainMod} + SHIFT + ${k}" "movetoworkspace ${toString i}"
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
        (dispatch "${mainMod} + SHIFT + W" "killactive")
        (exec "${mainMod} + SHIFT + E" "hyprland-graceful-exit")
        (dispatch "${mainMod} + SHIFT + semicolon" "togglefloating")
        (exec "${mainMod} + semicolon" ''hyprctl dispatch focuswindow $(if [[ $(hyprctl activewindow -j | jq ."floating") == "true" ]]; then echo "tiled"; else echo "floating"; fi;)'')
        (exec "${mainMod} + space" menu)
        (exec "ALT + space" menu)
        (exec "${mainMod} + SHIFT + Q" "${config.programs.hyprlock.package}/bin/hyprlock")
        (exec "${mainMod} + SHIFT + S" "${pkgs.hyprshot}/bin/hyprshot -m region")
        (exec "${mainMod} + E" "emacsclient -c")

        (exec "${mainMod} + O" "makoctl dismiss --all")

        (dispatch "${mainMod} + B" "movefocus l")
        (dispatch "${mainMod} + F" "movefocus r")
        (dispatch "${mainMod} + P" "movefocus u")
        (dispatch "${mainMod} + N" "movefocus d")
        (dispatch "${mainMod} + SHIFT + B" "movewindoworgroup l")
        (dispatch "${mainMod} + SHIFT + F" "movewindoworgroup r")
        (dispatch "${mainMod} + SHIFT + P" "movewindoworgroup u")
        (dispatch "${mainMod} + SHIFT + N" "movewindoworgroup d")

        (dispatch "${mainMod} + SHIFT + left" "movecurrentworkspacetomonitor l")
        (dispatch "${mainMod} + SHIFT + right" "movecurrentworkspacetomonitor r")
        (dispatch "${mainMod} + SHIFT + up" "movecurrentworkspacetomonitor u")
        (dispatch "${mainMod} + SHIFT + down" "movecurrentworkspacetomonitor d")

        (dispatch "${mainMod} + M" "fullscreen")
      ]
      ++ workspaceBinds
      ++ moveToWorkspaceBinds
      ++ [
        (dispatch "${mainMod} + mouse_down" "workspace e-1")
        (dispatch "${mainMod} + mouse_up" "workspace e+1")

        # Mouse drag/resize (Hyprlang bindm).
        (bindLua "${mainMod} + mouse:272" "hl.dsp.window.drag()" { mouse = true; })
        (bindLua "${mainMod} + mouse:273" "hl.dsp.window.resize()" { mouse = true; })

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
          "${hyprDpmsPhysical} off && hyprctl dispatch exec ${config.programs.hyprlock.package}/bin/hyprlock"
          { locked = true; }
        )
        (execOpts "switch:off:Lid Switch" "${hyprDpmsPhysical} on" { locked = true; })
      ];
  };
}
