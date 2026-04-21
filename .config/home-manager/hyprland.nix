{
  pkgs,
  ...
}:
{
  enable = true;
  package = null;
  settings = {
    general = {
      gaps_in = 0;
      gaps_out = 0;

      border_size = 1;
    };

    input = {
      repeat_delay = 200;
      repeat_rate = 60;
      resolve_binds_by_sym = 1;
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

    decoration = {
      shadow.enabled = false;
      blur.enabled = false;
    };

    animations = {
      enabled = "no";
    };

    misc = {
      disable_hyprland_logo = true;
      disable_splash_rendering = true;
      focus_on_activate = true;
    };

    "$terminal" = "ghostty";
    "$menu" = "walker";
    "$mainMod" = "SUPER";

    "exec-once" = [
      "dbus-update-activation-environment --systemd WAYLAND_DISPLAY HYPRLAND_INSTANCE_SIGNATURE XDG_CURRENT_DESKTOP"
      "kanshi"
      "hyprpanel"
      "elephant"
      "walker --gapplication-service"
    ];

    bind = [
      "$mainMod, return, exec, $terminal"
      "$mainMod SHIFT, W, killactive,"
      "$mainMod SHIFT, E, exec, hyprland-graceful-exit"
      "$mainMod SHIFT, space, togglefloating,"
      "$mainMod, space, exec, hyprctl dispatch focuswindow $(if [[ $(hyprctl activewindow -j | jq .\"floating\") == \"true\" ]]; then echo \"tiled\"; else echo \"floating\"; fi;)"
      "$mainMod, R, exec, $menu"
      # "$mainMod SHIFT, P, pseudo, # dwindle"
      # "$mainMod, V, togglesplit, # dwindle"
      # "$mainMod, G, togglegroup"
      "$mainMod SHIFT, Q, exec, hyprlock"
      "$mainMod SHIFT, S, exec, ${pkgs.hyprshot}/bin/hyprshot -m region"
      "$mainMod, E, exec, emacsclient -c"

      "$mainMod, O, exec, hyprpanel clearNotifications"

      "$mainMod, B, movefocus, l"
      "$mainMod, F, movefocus, r"
      "$mainMod, P, movefocus, u"
      "$mainMod, N, movefocus, d"
      "$mainMod SHIFT, B, movewindoworgroup, l"
      "$mainMod SHIFT, F, movewindoworgroup, r"
      "$mainMod SHIFT, P, movewindoworgroup, u"
      "$mainMod SHIFT, N, movewindoworgroup, d"

      "$mainMod SHIFT, left, movecurrentworkspacetomonitor, l"
      "$mainMod SHIFT, right, movecurrentworkspacetomonitor, r"
      "$mainMod SHIFT, up, movecurrentworkspacetomonitor, u"
      "$mainMod SHIFT, down, movecurrentworkspacetomonitor, d"

      "$mainMod, M, fullscreen"

      "$mainMod, 1, workspace, 1"
      "$mainMod, 2, workspace, 2"
      "$mainMod, 3, workspace, 3"
      "$mainMod, 4, workspace, 4"
      "$mainMod, 5, workspace, 5"
      "$mainMod, 6, workspace, 6"
      "$mainMod, 7, workspace, 7"
      "$mainMod, 8, workspace, 8"
      "$mainMod, 9, workspace, 9"
      "$mainMod, 0, workspace, 10"

      "$mainMod SHIFT, 1, movetoworkspace, 1"
      "$mainMod SHIFT, 2, movetoworkspace, 2"
      "$mainMod SHIFT, 3, movetoworkspace, 3"
      "$mainMod SHIFT, 4, movetoworkspace, 4"
      "$mainMod SHIFT, 5, movetoworkspace, 5"
      "$mainMod SHIFT, 6, movetoworkspace, 6"
      "$mainMod SHIFT, 7, movetoworkspace, 7"
      "$mainMod SHIFT, 8, movetoworkspace, 8"
      "$mainMod SHIFT, 9, movetoworkspace, 9"
      "$mainMod SHIFT, 0, movetoworkspace, 10"

      "$mainMod, mouse_down, workspace, e-1"
      "$mainMod, mouse_up, workspace, e+1"
    ];
    bindm = [
      "$mainMod, mouse:272, movewindow"
      "$mainMod, mouse:273, resizewindow"
    ];
    bindel = [
      ",XF86AudioRaiseVolume, exec, wpctl set-volume -l 1 @DEFAULT_AUDIO_SINK@ 5%+"
      ",XF86AudioLowerVolume, exec, wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-"
      ",XF86AudioMute, exec, wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"
      ",XF86AudioMicMute, exec, wpctl set-mute @DEFAULT_AUDIO_SOURCE@ toggle"
      ",XF86MonBrightnessUp, exec, ${pkgs.brightnessctl}/bin/brightnessctl -e4 -n2 set 5%+"
      ",XF86MonBrightnessDown, exec, ${pkgs.brightnessctl}/bin/brightnessctl -e4 -n2 set 5%-"
    ];
    bindl = [
      ", XF86AudioNext, exec, ${pkgs.playerctl}/bin/playerctl next"
      ", XF86AudioPause, exec, ${pkgs.playerctl}/bin/playerctl play-pause"
      ", XF86AudioPlay, exec, ${pkgs.playerctl}/bin/playerctl play-pause"
      ", XF86AudioPrev, exec, ${pkgs.playerctl}/bin/playerctl previous"

      ", switch:on:Lid Switch, exec, hyprctl dispatch dpms off && hyprctl dispatch exec hyprlock"
      ", switch:off:Lid Switch, exec, hyprctl dispatch dpms on"
    ];
  };
}
