"""Publisher metadata audit of the inherited bibliography; originals are preserved."""
import sys, json, re, difflib, time
from pathlib import Path
from urllib.request import urlopen, Request
from urllib.parse import urlencode, quote
from concurrent.futures import ThreadPoolExecutor
sys.path.insert(0, '/tmp/wechat_audit_deps')
import bibtexparser
root = Path(__file__).resolve().parents[1]
out = root / 'analysis/output/observational/verification'
out.mkdir(parents=True, exist_ok=True)
entries = {}
for f in ['writing/paper.bib', 'writing/references_additions.bib', 'teaching/references.bib']:
    for e in bibtexparser.loads((root / f).read_text()).entries:
        entries.setdefault(e['ID'], e)

def norm(s):
    return re.sub(r'[^a-z0-9 ]', '', s.lower()).strip()

def get(url):
    for i in range(3):
        try:
            with urlopen(Request(url, headers={'User-Agent':'WeChatTeachingBibliographyAudit/1.0'}), timeout=25) as r:
                return json.load(r)['message']
        except Exception:
            if i == 2: raise
            time.sleep(1+i)

def audit(e):
    result = {'key':e['ID'], 'original':e}
    try:
        if e.get('doi'):
            candidates = [get('https://api.crossref.org/works/' + quote(e['doi'], safe=''))]
        else:
            query = urlencode({'query.title':re.sub('[{}]', '', e['title']), 'rows':3})
            candidates = get('https://api.crossref.org/works?' + query)['items']
        ranked = sorted(candidates, key=lambda c:difflib.SequenceMatcher(None,norm(e['title']),norm(' '.join(c.get('title',[])))).ratio(), reverse=True)
        c=ranked[0]
        score=difflib.SequenceMatcher(None,norm(e['title']),norm(' '.join(c.get('title',[])))).ratio()
        result.update(score=score, candidate=c, status='metadata_match' if score>.94 else 'manual_review')
    except Exception as exc:
        result.update(status='unresolved', error=str(exc))
    return result

targets = [e for e in entries.values() if e['ENTRYTYPE']=='article']
with ThreadPoolExecutor(max_workers=6) as pool:
    records=list(pool.map(audit, targets))
(out/'bibliography_metadata.json').write_text(json.dumps(records,ensure_ascii=False,indent=2))
lines=['# Inherited bibliography metadata audit','',
       'Crossref title/DOI matches verify bibliographic metadata, not substantive claims. Book and web sources require separate checks. Original files are preserved.','',
       '| Key | Status | Match | Original year | Print / online years | DOI |','|---|---|---:|---:|---|---|']
for r in records:
    c=r.get('candidate',{}); years=[]
    for k in ['published-print','published-online','published']:
        if k in c: years.append(k+': '+str(c[k]['date-parts'][0][0]))
    lines.append('| '+ ' | '.join([r['key'],r['status'],f"{r.get('score',0):.3f}",r['original'].get('year',''),', '.join(years),c.get('DOI','')])+' |')
(out/'bibliography_metadata.md').write_text('\n'.join(lines)+'\n')
print('\n'.join(lines))
