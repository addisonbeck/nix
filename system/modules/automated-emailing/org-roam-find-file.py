#!/usr/bin/env python3
import os
import re
import sys
import logging
from glob import glob

LOGFILE = "/var/lib/cookbook-to-kindle/cookbook-stitch.log"
ROAM_DIR = os.path.expanduser("~/Library/Mobile Documents/com~apple~CloudDocs/notes/roam")
COOKBOOK_ID = "9188213C-A1ED-4559-81CF-606695905B2D"

def setup_logger():
    logging.basicConfig(filename=LOGFILE, filemode='a', level=logging.DEBUG,
                        format='[%(asctime)s] %(message)s')
    # Log to stderr as well
    logging.getLogger().addHandler(logging.StreamHandler(sys.stderr))
    logging.info("==== BEGIN python org-roam-find-file ====")

def read_file(path):
    with open(path, encoding="utf-8") as f:
        return f.read()

def write_file(path, content):
    with open(path, "w", encoding="utf-8") as f:
        f.write(content)

def grep_id_file(roam_id):
    pat = f':ID: {roam_id}'
    logging.debug(f"Searching for roam node with id={roam_id}")
    # Look specifically for the file where this is the main ID (appears early in file)
    for org_file in sorted(glob(os.path.join(ROAM_DIR, "*.org"))):
        with open(org_file, encoding="utf-8") as fh:
            content = fh.read()
            # Check if this ID appears in the first 300 characters (in the properties drawer)
            if pat in content[:300]:
                logging.info(f"Found node {roam_id} as primary ID at {org_file}")
                return org_file
    
    # Fallback: look for the pattern anywhere in any file
    for org_file in sorted(glob(os.path.join(ROAM_DIR, "*.org"))):
        with open(org_file, encoding="utf-8") as fh:
            content = fh.read()
            if pat in content:
                logging.info(f"Found node {roam_id} at {org_file} (fallback)")
                return org_file
    logging.warning(f"Node id={roam_id} not found in roam dir")
    return None

def parse_headings_and_links(org_src):
    lines = org_src.splitlines(keepends=True)
    results = []
    link_re = re.compile(r'\[\[id:([A-F0-9\-]+)\]\]')
    for idx, line in enumerate(lines):
        m = link_re.search(line)
        if m:
            results.append(('link', m.group(1), idx, line))
        elif line.strip().startswith('*'):
            results.append(('heading', None, idx, line))
        else:
            results.append(('other', None, idx, line))
    return results

def strip_properties_and_title(org_txt):
    # Remove property drawers, :PROPERTIES: ... :END:, and the first #+TITLE: line
    out = []
    skip = False
    for line in org_txt.splitlines():
        if line.strip().startswith("#+TITLE:"): continue
        #if line.strip() == ":PROPERTIES:":
            #skip = True
            #continue
        #if skip and line.strip() == ":END:":
            #skip = False
            #continue
        #if skip: continue
        out.append(line)
    return '\n'.join(out) + '\n'

def get_head_depth(line):
    m = re.match(r'(\*+)', line)
    if m:
        return len(m.group(1))
    return 0

def demote_headings(org_txt, base_level):
    lines = org_txt.split('\n')
    heading_pat = re.compile(r'^(\*+)\s')
    out = []
    min_level = None
    # Determine minimal heading level present
    for line in lines:
        m = heading_pat.match(line)
        if m:
            l = len(m.group(1))
            if min_level is None or l < min_level:
                min_level = l
    if min_level is None:
        return org_txt  # No heading, nothing to do

    # Compute the increment needed
    shift = max(base_level - min_level, 0)
    for line in lines:
        m = heading_pat.match(line)
        if m:
            out.append(('*' * (len(m.group(1)) + shift)) + line[m.end()-1:])
        else:
            out.append(line)
    return '\n'.join(out)

def expand_link(roam_id, context_level):
    filename = grep_id_file(roam_id)
    if not filename:
        return f'# Could not find recipe for id: {roam_id}\n'
    content = read_file(filename)
    body = strip_properties_and_title(content)
    first_heading = next((ln for ln in body.splitlines() if ln.strip().startswith('*')), None)
    heading_level = get_head_depth(first_heading) if first_heading else 1
    demoted = demote_headings(body, context_level)
    logging.info(f"Expanded id={roam_id}: demoted by {context_level - heading_level}, level={context_level}")
    return demoted

def build_org_cookbook(template_path):
    template = read_file(template_path)
    results = []
    link_re = re.compile(r'\[\[id:([A-F0-9\-]+)\]\[[^\]]*\]\]')
    for line in template.splitlines():
        logging.info(f"Reading line {line!r}")
        link_match = link_re.search(line)
        logging.info(f"Line is link? {bool(link_match)}")
        if link_match:
            roam_id = link_match.group(1)
            base_level = get_head_depth(line) or 1
            # Only expand links that are at level 3 (*** headings) - these are the actual recipes
            # This limits expansion to "one level down" from the main cookbook structure
            if base_level == 3:
                logging.info(f"Expanding level 3 recipe link: {roam_id}")
                expanded = expand_link(roam_id, base_level)
                results.append(expanded)
            else:
                logging.info(f"Skipping expansion of level {base_level} link: {roam_id}")
                results.append(line)
        else:
            results.append(line)
    logging.info("Finished building cookbook content with links expanded")
    return '\n'.join(results) + '\n'

def main():
    setup_logger()
    template_path = grep_id_file(COOKBOOK_ID)
    if not template_path:
        logging.error("Cookbook org file not found!")
        sys.exit(1)
    logging.info(f"Using template: {template_path}")
    out_path = "/var/lib/cookbook-to-kindle/cookbook.org"
    write_file(out_path, "" + build_org_cookbook(template_path))
    print(out_path)
    logging.info(f"Wrote output to {out_path}.")
    logging.info("==== END python org-roam-find-file ====")

if __name__ == '__main__':
    main()
