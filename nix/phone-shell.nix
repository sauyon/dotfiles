# `nix develop .#phone` -- tools for debugging an Android phone over USB.
#
# A dev shell rather than home.packages: this is reached for a few times a year,
# and adb in particular starts a background server the moment it is run, which
# is not something to leave lying on every host's PATH.
#
# The phone has to offer itself first: Settings -> Developer options -> USB
# debugging, then accept the host's key on the prompt that appears at the first
# `adb devices`. A device listed as `unauthorized` is that prompt going
# unanswered; `no permissions` is udev, not the phone.
{ pkgs }:

pkgs.mkShell {
  packages = [
    # adb and fastboot. `adb logcat -b all` for the live log, `adb bugreport`
    # for the whole state as one zip, `adb shell dumpsys <service>` for one
    # subsystem (wifi, connectivity, bluetooth_manager, telephony.registry).
    pkgs.android-tools
    # Mirrors and drives the screen, so a setting can be flipped or a failure
    # watched without picking the phone up mid-capture.
    pkgs.scrcpy
    # tshark, for the Bluetooth side: with "Enable Bluetooth HCI snoop log" on,
    # a bugreport carries btsnoop_hci.log, and `tshark -r` reads it -- which is
    # where a dropped link shows who sent the disconnect and with what reason.
    pkgs.wireshark-cli
    # dumpsys and bugreports are increasingly JSON-ish; filtering them by hand
    # is how the one relevant line gets missed.
    pkgs.jq
  ];

  shellHook = ''
    echo "phone shell: adb, fastboot, scrcpy, tshark, jq"
    echo "  adb devices -l                  is the phone seen, and authorised"
    echo "  adb logcat -b all -v threadtime live log, every buffer"
    echo "  adb bugreport <dir>             full state, incl. the HCI snoop log"
  '';
}
