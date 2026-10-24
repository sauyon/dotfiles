"""Subset a font down to the codepoints no other font has.

    font-gaps.py <font> <out> <font-dir>...

Every face under each <font-dir> (.otf/.ttf, and each face of a .ttc) counts
as already covering its cmap; <out> keeps only <font>'s codepoints outside
that union, so fontconfig can only ever reach it for a glyph that would
otherwise be tofu. home.nix uses it for Noto Sans Symbols 2, which otherwise
outranks Noto Color Emoji and the profile's text fonts for glyphs they had.
"""
import os
import sys

from fontTools import subset
from fontTools.ttLib import TTCollection, TTFont


def faces(path):
    if path.lower().endswith(".ttc"):
        return TTCollection(path, lazy=True).fonts
    return [TTFont(path, lazy=True)]


def main():
    font, out, *dirs = sys.argv[1:]
    have = set(TTFont(font).getBestCmap())
    taken = set()
    for d in dirs:
        found = 0
        for root, _, files in os.walk(d, followlinks=True):
            for f in files:
                if f.lower().endswith((".otf", ".ttf", ".ttc")):
                    found += 1
                    for face in faces(os.path.join(root, f)):
                        taken |= set(face.getBestCmap() or {})
        # A package that moved its fonts would otherwise count as covering
        # nothing, and this font would quietly take its glyphs back.
        if not found:
            sys.exit(f"font-gaps: no fonts under {d}")
    keep = sorted(have - taken)
    if not keep:
        sys.exit(f"font-gaps: nothing left of {font} once {dirs} are taken")
    print(f"font-gaps: {len(have)} codepoints, {len(have & taken)} already covered, keeping {len(keep)}")
    opts = subset.Options()
    opts.name_IDs = ["*"]
    opts.layout_features = ["*"]
    opts.notdef_outline = True
    sub = subset.Subsetter(opts)
    sub.populate(unicodes=keep)
    f = TTFont(font)
    sub.subset(f)
    os.makedirs(os.path.dirname(out), exist_ok=True)
    f.save(out)


if __name__ == "__main__":
    main()
