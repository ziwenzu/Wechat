"""Compile the teaching manuscript and remove only its intermediate files."""
from pathlib import Path
import subprocess

folder = Path(__file__).resolve().parent
stem = "Teaching_Paper"
result = subprocess.run(
    ["latexmk", "-pdf", "-interaction=nonstopmode", "-halt-on-error", stem + ".tex"],
    cwd=folder, capture_output=True, text=True,
)
log_folder = folder.parent / "analysis" / "output" / "teaching" / "logs"
log_folder.mkdir(parents=True, exist_ok=True)
(log_folder / "latex-build.txt").write_text(result.stdout + result.stderr)
if result.returncode:
    raise SystemExit("Compilation failed; diagnostics are saved in analysis/output/teaching/logs/latex-build.txt")
(log_folder / "latex-final.txt").write_text((folder / (stem + ".log")).read_text())
subprocess.run(["latexmk", "-c", stem + ".tex"], cwd=folder, capture_output=True, check=True)
for suffix in (".aux", ".bbl", ".bcf", ".blg", ".fdb_latexmk", ".fls", ".log", ".out",
               ".ptc", ".run.xml", ".synctex.gz", "-blx.bib"):
    (folder / (stem + suffix)).unlink(missing_ok=True)
print("Teaching manuscript compiled and intermediate files removed.")
