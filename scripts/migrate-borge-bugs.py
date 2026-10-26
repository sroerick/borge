#!/usr/bin/env python3
"""Convert .borge-bug sexp files to a single markdown archive.

Why this exists: `borge issue` / `lib/bug` are deleted in Phase E of
docs/orchestration-deprecation.borg. The .borge-bug files are plain sexp and
stay on disk, but once the reader is gone nothing in the repo can render them
as prose, and a future reader would have to hand-parse sexp to learn what was
filed. This dumps them to markdown once, so the history stays readable.

Usage:
    migrate-borge-bugs.py <bugs-dir> [--out FILE]

Reads every *.borge-bug in <bugs-dir>, writes markdown to --out (default:
stdout). Does not modify or delete the source files.
"""

import sys
import os
import glob

# --- minimal sexp reader -------------------------------------------------
# Enough for .borge-bug: atoms, quoted strings with escapes, verbatim
# strings delimited by (| ... |), and nested lists. Comments are not used
# in this format.

class Sym(str):
    """An unquoted atom, kept distinct so we can tell `closed` from "closed"."""


def tokenize(text):
    i, n = 0, len(text)
    while i < n:
        c = text[i]
        if c in ' \t\r\n':
            i += 1
        elif c == ';':
            while i < n and text[i] != '\n':
                i += 1
        elif c == '(':
            # (| ... |) verbatim literal
            if text[i:i + 2] == '(|':
                end = text.find('|)', i + 2)
                if end < 0:
                    raise ValueError('unterminated verbatim at %d' % i)
                yield ('v', text[i + 2:end])
                i = end + 2
            else:
                yield ('(', None)
                i += 1
        elif c == ')':
            yield (')', None)
            i += 1
        elif c == '"':
            j = i + 1
            buf = []
            while j < n:
                if text[j] == '\\' and j + 1 < n:
                    nxt = text[j + 1]
                    buf.append({'n': '\n', 't': '\t', '"': '"', '\\': '\\'}.get(nxt, nxt))
                    j += 2
                elif text[j] == '"':
                    break
                else:
                    buf.append(text[j])
                    j += 1
            else:
                raise ValueError('unterminated string at %d' % i)
            yield ('s', ''.join(buf))
            i = j + 1
        else:
            j = i
            while j < n and text[j] not in ' \t\r\n()':
                j += 1
            yield ('a', text[i:j])
            i = j


def parse(text):
    stack = [[]]
    for kind, val in tokenize(text):
        if kind == '(':
            stack.append([])
        elif kind == ')':
            done = stack.pop()
            stack[-1].append(done)
        elif kind == 'v':
            stack[-1].append(val)          # verbatim: plain string
        elif kind == 's':
            stack[-1].append(val)          # quoted: plain string
        else:
            stack[-1].append(Sym(val))
    return stack[0]


def field(forms, name):
    """Return the first child list whose head atom is `name`."""
    for f in forms:
        if isinstance(f, list) and f and isinstance(f[0], Sym) and f[0] == name:
            return f
    return None


def atom(forms, name, default=''):
    f = field(forms, name)
    if f is None or len(f) < 2:
        return default
    v = f[1]
    return v if isinstance(v, str) else default


def body(forms, name):
    f = field(forms, name)
    if f is None or len(f) < 2:
        return ''
    # (doc "...") or (doc (||)) or (doc (| ... |))
    v = f[1]
    if isinstance(v, str):
        return v.strip()
    if isinstance(v, list):
        return ' '.join(x for x in v if isinstance(x, str)).strip()
    return ''


def lenient_fields(text):
    """Best-effort field extraction for specs the strict reader rejects.

    A .borge-bug written by `borge issue close` can contain unescaped double
    quotes inside its (doc "...") body — which is exactly the corruption
    BORGE-1783789294 describes. Dropping such a record from the archive would
    lose the most interesting bug in the set, so fall back to line-oriented
    extraction and flag the record instead.
    """
    import re
    def grab(name):
        m = re.search(r'\(%s\s+"([^"\n]*)"' % re.escape(name), text)
        if m:
            return m.group(1)
        m = re.search(r'\(%s\s+([^\s()]+)' % re.escape(name), text)
        return m.group(1) if m else ''
    # The doc body runs from (doc " to the closing " before (resolution.
    m = re.search(r'\(doc\s+"(.*)"\s*\)\s*\n?\s*\(resolution', text, re.S)
    doc = m.group(1).strip() if m else ''
    return {
        'id': grab('id'),
        'title': grab('title'),
        'status': grab('status') or 'unknown',
        'created': grab('created'),
        'filed_by': grab('filed-by'),
        'doc': doc,
        'malformed': True,
    }


def main():
    if len(sys.argv) < 2:
        sys.exit(__doc__)
    bugs_dir = sys.argv[1]
    out = sys.stdout
    if '--out' in sys.argv:
        out = open(sys.argv[sys.argv.index('--out') + 1], 'w')

    files = sorted(glob.glob(os.path.join(bugs_dir, '*.borge-bug')))
    if not files:
        print('no .borge-bug files in %s' % bugs_dir, file=sys.stderr)
        return

    rows = []
    for path in files:
        raw = open(path).read()
        try:
            forms = parse(raw)
        except Exception as e:
            rec = lenient_fields(raw)
            rec.setdefault('id', os.path.basename(path).replace('.borge-bug', ''))
            if not rec['id']:
                rec['id'] = os.path.basename(path).replace('.borge-bug', '')
            print('LENIENT %s: %s' % (path, e), file=sys.stderr)
            rows.append(rec)
            continue
        top = forms[0] if forms and isinstance(forms[0], list) else []
        rows.append({
            'id': atom(top, 'id', os.path.basename(path).replace('.borge-bug', '')),
            'title': atom(top, 'title', '(no title)'),
            'status': atom(top, 'status', 'unknown'),
            'created': atom(top, 'created', ''),
            'filed_by': atom(top, 'filed-by', ''),
            'doc': body(top, 'doc'),
            'malformed': False,
        })

    order = {'open': 0, 'triage': 1, 'acknowledged': 2, 'plan-approved': 3, 'closed': 4}
    rows.sort(key=lambda r: (order.get(r['status'], 9), r['id']))

    print('# Migrated borge issues\n', file=out)
    print('Archived from `%s` (%d records) before `borge issue` and '
          '`lib/bug` were retired.\n' % (bugs_dir, len(rows)), file=out)
    print('Source files are unchanged; this is a readable projection of them.\n', file=out)

    counts = {}
    for r in rows:
        counts[r['status']] = counts.get(r['status'], 0) + 1
    print('| status | count |', file=out)
    print('|---|---|', file=out)
    for k in sorted(counts, key=lambda s: order.get(s, 9)):
        print('| %s | %d |' % (k, counts[k]), file=out)
    print('', file=out)

    for r in rows:
        print('---\n', file=out)
        print('## %s — %s\n' % (r['id'], r['title']), file=out)
        print('- **status:** %s' % r['status'], file=out)
        if r['created']:
            print('- **created:** %s' % r['created'], file=out)
        if r.get('malformed'):
            print('- **note:** source record is malformed (unescaped quotes in its '
                  'doc body); fields recovered leniently', file=out)
        if r['filed_by']:
            print('- **filed by:** %s' % r['filed_by'], file=out)
        print('', file=out)
        if r['doc']:
            for line in r['doc'].split('\n'):
                print(line, file=out)
            print('', file=out)


if __name__ == '__main__':
    main()
