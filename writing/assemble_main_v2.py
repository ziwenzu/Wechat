# Assemble Main_v2.tex from the original preamble/SI, the new body, theory_v2 and the notice.
import io, sys
sys.path.insert(0, '.')
from si_cleanup import clean
orig = io.open('Main_original.tex', encoding='utf-8').read().split('\n')
preamble = orig[0:78]; spacing = orig[112:118]; bib = orig[512:520]; si_a = orig[525:688]; si_b = orig[688:973]
body   = io.open('Main_v2_body.tex', encoding='utf-8').read()
theory = io.open('theory_v2.tex', encoding='utf-8').read()
notice = io.open('simulation_notice.tex', encoding='utf-8').read()
body = body.replace('\\input{theory_v2}', theory)
front = io.open('Main_v2_front.tex', encoding='utf-8').read()
si_new = io.open('Main_v2_si_additions.tex', encoding='utf-8').read()
si_b_text = clean('\n'.join(si_b))
anchor = "Figure~\\ref{fig:extensive-margin-ts} plots the monthly extensive margin"
pooled_fig = io.open('Main_v2_si_pooled_fig.tex', encoding='utf-8').read()
assert anchor in si_b_text
si_b_text = si_b_text.replace(anchor, pooled_fig + anchor, 1)
out = ('\n'.join(preamble) + '\n' + front.replace('%%NOTICE%%', notice) + '\n' + '\n'.join(spacing) + '\n\n' + body + '\n\n'
       + '\n'.join(bib) + '\n\n' + clean('\n'.join(si_a)) + '\n' + si_new + si_b_text + '\n\n\\end{document}\n')
io.open('Main_v2.tex', 'w', encoding='utf-8').write(out)
print('Main_v2.tex written:', len(out.split('\n')), 'lines')
