"""Build the Claude-based edition and keep diagnostics outside the manuscript folder."""
from pathlib import Path
import subprocess, shutil

root = Path(__file__).resolve().parents[1]
writing = root / 'writing'
logs = root / 'analysis/output/observational/verification'
logs.mkdir(parents=True, exist_ok=True)
with (logs / 'latex_build.log').open('w') as stream:
    result = subprocess.run(
        ['latexmk','-pdf','-interaction=nonstopmode','-halt-on-error','Main_restructured.tex'],
        cwd=writing, stdout=stream, stderr=subprocess.STDOUT)
if (writing / 'Main_restructured.log').exists():
    shutil.copy2(writing / 'Main_restructured.log', logs / 'latex_final.log')
if result.returncode:
    raise SystemExit('Compilation failed; see analysis/output/observational/verification/latex_build.log')
for path in writing.glob('Main_restructured.*'):
    if path.suffix not in {'.tex','.pdf'}:
        path.unlink()
print('Built writing/Main_restructured.pdf; diagnostics saved and intermediate files cleared.')
