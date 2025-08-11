#!/usr/bin/env python3
import os
import sys
import argparse
from glob import glob

DEFAULT_ROAM_DIR = os.path.expanduser("~/Library/Mobile Documents/com~apple~CloudDocs/notes/roam")

def find_by_id(roam_id, roam_dir):
    pat = f":ID: {roam_id}"
    for org in glob(os.path.join(roam_dir, "*.org")):
        try:
            with open(org, encoding="utf-8") as fh:
                if pat in fh.read():
                    return os.path.abspath(org)
        except Exception:
            continue
    return None

def main():
    ap = argparse.ArgumentParser(description="Resolve an Org-roam ID or file path to an absolute .org file path.")
    g = ap.add_mutually_exclusive_group(required=True)
    g.add_argument("--id", help="Org-roam ID to resolve")
    g.add_argument("--file", help="Path to an org file")
    ap.add_argument("--roam-dir", default=DEFAULT_ROAM_DIR, help="Org-roam directory to search")
    args = ap.parse_args()

    if args.id:
        path = find_by_id(args.id, args.roam_dir)
        if not path:
            print(f"Error: Roam ID not found: {args.id}", file=sys.stderr)
            sys.exit(2)
        print(path)
        return

    if args.file:
        path = os.path.abspath(os.path.expanduser(args.file))
        if not os.path.exists(path):
            print(f"Error: File not found: {path}", file=sys.stderr)
            sys.exit(3)
        if not path.endswith(".org"):
            print(f"Error: Not an .org file: {path}", file=sys.stderr)
            sys.exit(4)
        print(path)
        return

if __name__ == "__main__":
    main()
