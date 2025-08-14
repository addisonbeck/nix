#!/usr/bin/env python3
import os, sys
from glob import glob

ROAM_DIR = os.path.expanduser("~/Library/Mobile Documents/com~apple~CloudDocs/notes/roam")

def find_file_by_id(roam_id: str):
    pat = f":ID: {roam_id}"
    for org_file in glob(os.path.join(ROAM_DIR, "*.org")):
        try:
            with open(org_file, encoding="utf-8") as fh:
                if pat in fh.read():
                    return org_file
        except Exception:
            continue
    return None

def main():
    if len(sys.argv) != 2:
        print("usage: org-roam-find-node-file.py ROAM_ID", file=sys.stderr)
        sys.exit(1)
    roam_id = sys.argv[1].strip()
    if roam_id.startswith("id:"):
        roam_id = roam_id[3:]
    path = find_file_by_id(roam_id)
    if not path:
        print(f"not-found: {roam_id}", file=sys.stderr)
        sys.exit(2)
    print(path)

if __name__ == "__main__":
    main()
